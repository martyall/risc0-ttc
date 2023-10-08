module Main
  ( AppData
  , TokenId
  , Users
  , awaitTTCResult
  , buyTokens
  , closeRankings
  , closeSubmissions
  , defaultTTCTxOpts
  , defaultTokenTxOpts
  , main
  , mkAppData
  , rankTokens
  , ranks
  , retrieveTokens
  , unsafeToUInt
  , verifyPreferences
  ) where

import Prelude

import Chanterelle.Deploy (deploy, readDeployAddress)
import Chanterelle.Logging (setLogLevel, LogLevel(..))
import Chanterelle.Test (assertWeb3, takeEvent)
import Chanterelle.Utils (pollTransactionReceipt)
import Contracts.TTCTrading as TTC
import Contracts.Token as Token
import Control.Parallel (parTraverse, parTraverse_)
import Cycle (applyCycles, mkCycle)
import Data.Array (length, (!!), (..))
import Data.Either (Either(..), either)
import Data.Homogeneous.Record (fromHomogeneous, homogeneous)
import Data.Int (fromString)
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (for, for_, sequence, traverse)
import Data.Tuple (Tuple(..), fst)
import Deploy.ContractConfig (tokenLibCfg, ttcLibCfg)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, throwError)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Effect.Random (randomInt)
import Matrix (formatMatrix)
import Network.Ethereum.Core.BigNumber (BigNumber)
import Network.Ethereum.Core.HexString (unHex)
import Network.Ethereum.Core.HexString as HexString
import Network.Ethereum.Core.Signatures (unAddress)
import Network.Ethereum.Types (Address, HexString, fromInt)
import Network.Ethereum.Web3 (ChainCursor(..), EventAction(..), Provider, TransactionOptions, TransactionReceipt(..), TransactionStatus(..), UIntN, Web3, _from, _gas, _to, defaultTransactionOptions, eventFilter, httpProvider, runWeb3, uIntNFromBigNumber)
import Network.Ethereum.Web3.Api (eth_getAccounts, net_version)
import Network.Ethereum.Web3.Contract.Events (pollEvent')
import Network.Ethereum.Web3.Solidity (unVector)
import Network.Ethereum.Web3.Types (NoPay)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = setLogLevel Debug *> launchAff_ do

  appData <- mkAppData
  t <- buyTokens appData
  Console.log $ "bought tokens: " <> show t
  transferTokensToTTC appData t
  Console.info "Transfered tokens to TTC"
  _ <- closeSubmissions appData
  Console.info "Sealed the submissions"
  let ranking = ranks t
  displayRankings ranking
  rankTokens appData ranking
  Console.info "Ranked the tokens"
  verifyPreferences appData ranking
  Console.info "Verified the preferences in contract"
  closeRankings appData
  -- NOTE: If you want to continue by using the relay manually, end
  -- the program now and use the raw logs as input
  Console.info "Emitted Data to Relay"
  awaitTTCResult appData
  Console.info "Retrieving Tokens"
  trades <- retrieveTokens appData t
  displayTrades trades
  verifyOutcomes trades
  Console.info "Resetting contract"
  resetContract appData
  Console.info "Done"

type Users a =
  { user0 :: a
  , user1 :: a
  , user2 :: a
  , user3 :: a
  , user4 :: a
  , user5 :: a
  }

type AppData =
  { token :: Address
  , ttc :: Address
  , primaryAccount :: Address
  , users :: Users Address
  , provider :: Provider
  }

type TokenId = UIntN 256

mkAppData :: Aff AppData
mkAppData = do
  let nodeURL = "http://localhost:8545"
  provider <- liftEffect $ httpProvider nodeURL
  networkId <- do
    version <- liftAff $ assertWeb3 provider net_version
    case fromString version of
      Nothing -> throwError $ error $ "Unknown network version: " <> version
      Just v -> pure v
  { token, ttc } <- deploy nodeURL 10 do
    token <- readDeployAddress tokenLibCfg networkId
    ttc <- readDeployAddress ttcLibCfg networkId
    pure { token, ttc }
  eusers <- runWeb3 provider getAccounts
  users <- either (throwError <<< error <<< show) pure eusers
  let primaryAccount = users.user1
  pure { provider, users, ttc, token, primaryAccount }
  where
  getAccounts :: Web3 (Users Address)
  getAccounts = do
    a <- eth_getAccounts
    let
      accounts = map fromHomogeneous $ sequence $ homogeneous
        { user0: a !! 0
        , user1: a !! 1
        , user2: a !! 2
        , user3: a !! 3
        , user4: a !! 4
        , user5: a !! 5
        }
    pure $ unsafePartial fromJust $ accounts

buyTokens
  :: AppData
  -> Aff (Users { user :: Address, tokenId :: TokenId })
buyTokens appData = do
  let users = homogeneous appData.users
  tokenIds <- for users $ \user -> do
    tokenId <- unsafeToUInt <$> liftEffect (randomInt 1 10000)
    pure { user, tokenId }
  let
    f { user, tokenId } = assertWeb3 appData.provider do
      let
        txOpts = defaultTokenTxOpts appData #
          _from ?~ user
        action = Token.mintToken txOpts { to: user, tokenId }
      void $ takeEvent (Proxy @Token.Transfer) appData.token action
  parTraverse_ f tokenIds
  pure $ fromHomogeneous tokenIds

transferTokensToTTC
  :: AppData
  -> Users { user :: Address, tokenId :: TokenId }
  -> Aff Unit
transferTokensToTTC appData _users = do
  let
    users = homogeneous _users
    f { user, tokenId } = assertWeb3 appData.provider do
      let
        tokenOpts = defaultTokenTxOpts appData #
          _from ?~ user
        approval = Token.approve tokenOpts { to: appData.ttc, tokenId }
      void $ takeEvent (Proxy @Token.Approval) appData.token approval
      let
        ttcOpts = defaultTTCTxOpts appData #
          _from ?~ user
        action = TTC.submitToken ttcOpts { _tokenId: tokenId }
      void $ takeEvent (Proxy @Token.Transfer) appData.token action
  parTraverse_ f users

closeSubmissions :: AppData -> Aff HexString
closeSubmissions appData = assertWeb3 appData.provider $ do
  let
    txOpts = defaultTTCTxOpts appData #
      _from ?~ appData.primaryAccount
    action = TTC.sealTokensAndStartRanking txOpts
  fst <$> takeEvent (Proxy @TTC.PhaseChanged) appData.ttc action

ranks
  :: Users { user :: Address, tokenId :: TokenId }
  -> Users { user :: Address, tokenId :: TokenId, prefs :: Array TokenId }
ranks users =
  let
    { user0: { user: user0, tokenId: token0 }
    , user1: { user: user1, tokenId: token1 }
    , user2: { user: user2, tokenId: token2 }
    , user3: { user: user3, tokenId: token3 }
    , user4: { user: user4, tokenId: token4 }
    , user5: { user: user5, tokenId: token5 }
    } = users
  in
    { user0: { user: user0, tokenId: token0, prefs: [ token2, token1, token3, token0 ] }
    , user1: { user: user1, tokenId: token1, prefs: [ token2, token4, token5 ] }
    , user2: { user: user2, tokenId: token2, prefs: [ token2, token0 ] }
    , user3: { user: user3, tokenId: token3, prefs: [ token1, token4, token5, token3 ] }
    , user4: { user: user4, tokenId: token4, prefs: [ token0, token2, token1 ] }
    , user5: { user: user5, tokenId: token5, prefs: [ token1, token3, token4, token5 ] }
    }

displayRankings
  :: Users { user :: Address, tokenId :: TokenId, prefs :: Array TokenId }
  -> Aff Unit
displayRankings _users = do
  let users = homogeneous _users
  for_ users $ \{ user, tokenId, prefs } ->
    Console.info $ "User " <> formatAddress user <> ": " <> show tokenId <> " ==> " <> show prefs

rankTokens
  :: forall r
   . AppData
  -> Users { user :: Address, prefs :: Array TokenId | r }
  -> Aff Unit
rankTokens appData _users = do
  let
    users = homogeneous _users
    f { user, prefs } = do
      let
        txOpts = defaultTTCTxOpts appData #
          _from ?~ user
      txHash <- assertWeb3 appData.provider $ TTC.submitPreferences txOpts { _preferenceList: prefs }
      TransactionReceipt { status } <- pollTransactionReceipt txHash appData.provider
      when (status /= Succeeded) $ throwError $ error $ "rankTokens transaction failed: " <> show txHash
  parTraverse_ f users

verifyPreferences
  :: AppData
  -> Users { user :: Address, tokenId :: TokenId, prefs :: Array TokenId }
  -> Aff Unit
verifyPreferences appData _users = do
  let users = homogeneous _users
  let
    f { user, prefs } = assertWeb3 appData.provider $ do
      let
        txOpts = defaultTTCTxOpts appData
        ownerIdxs = (0 .. 5)
      for_ ownerIdxs $ \ix -> do
        eowner <- TTC.ownersArray txOpts Latest (unsafeToUInt ix)
        owner <- either (throwError <<< error <<< show) pure eowner
        when (owner == user) $ do
          let prefIdxs = (0 .. (length prefs - 1))
          for_ prefIdxs \prefIndex -> do
            pref <- TTC.preferenceListsArray txOpts Latest $ { _1: unsafeToUInt ix, _2: unsafeToUInt prefIndex }
            let trueVal = prefs !! prefIndex
            unless (Just pref == map Right trueVal)
              $ throwError
              $ error
              $ "For index " <> show ix <> ", " <> show prefIndex
                  <> "wanted "
                  <> show trueVal
                  <> " but got "
                  <> show pref
  parTraverse_ f users

closeRankings
  :: AppData
  -> Aff Unit
closeRankings appData = assertWeb3 appData.provider $ do
  let
    txOpts = defaultTTCTxOpts appData #
      _from ?~ appData.primaryAccount
    action = TTC.lockRankingAndExecuteTTC txOpts
  Tuple _ ev <- takeEvent (Proxy @TTC.TokenDetailsEmitted) appData.ttc action
  let TTC.TokenDetailsEmitted { tokenIds, preferenceLists } = ev
  Console.info $
    "Corresponds to preference matrix: \n" <>
      formatMatrix { header: unVector tokenIds, matrix: unVector preferenceLists }

awaitTTCResult
  :: AppData
  -> Aff Unit
awaitTTCResult appData =
  let
    monitor = \(TTC.TTCResult { result }) -> do
      Console.info $ "Found trading cycles: " <> show result
      pure TerminateEvent
    filter = eventFilter (Proxy :: Proxy TTC.TTCResult) appData.ttc
  in
    void $ assertWeb3 appData.provider $ pollEvent' { f: filter } { f: monitor }

retrieveTokens
  :: forall r
   . AppData
  -> Users { user :: Address, tokenId :: TokenId | r }
  -> Aff (Users { user :: Address, tokenId :: TokenId, trade :: TokenId })
retrieveTokens appData _users = do
  let
    users = homogeneous _users
    f { user, tokenId } = assertWeb3 appData.provider $ do
      let
        ttcOpts = defaultTTCTxOpts appData #
          _from ?~ user
        action = TTC.retrieveToken ttcOpts
      Tuple _ (Token.Transfer t) <- takeEvent (Proxy @Token.Transfer) appData.token action
      pure { user, tokenId, trade: t.tokenId }
  res <- parTraverse f users
  pure $ fromHomogeneous res

verifyOutcomes
  :: forall r
   . Users { user :: Address, tokenId :: TokenId, trade :: TokenId | r }
  -> Aff Unit
verifyOutcomes users = do
  let us = homogeneous users
  for_ us $ \{ user, tokenId, trade } -> do
    let expectedTrade = applyCycles expected tokenId
    when (expectedTrade /= Just trade)
      $ liftEffect
      $ throw
      $ "Unexpected trade for user " <> formatAddress user <> ":\n"
          <> "expected "
          <> show expectedTrade
          <> " but got "
          <> show trade
  Console.log "Verified outcomes"
  where
  expected = unsafePartial $ fromJust $ traverse mkCycle
    [ [ users.user0.tokenId, users.user1.tokenId, users.user4.tokenId ]
    , [ users.user2.tokenId ]
    , [ users.user3.tokenId, users.user5.tokenId ]
    ]

displayTrades
  :: Users { user :: Address, tokenId :: TokenId, trade :: TokenId }
  -> Aff Unit
displayTrades _users = do
  let users = homogeneous _users
  for_ users $ \{ user, tokenId, trade } ->
    Console.log $ "User " <> formatAddress user <> ": " <> show tokenId <> " ==> " <> show trade

resetContract
  :: AppData
  -> Aff Unit
resetContract appData = void $ runWeb3 appData.provider do
  Console.log "Resetting Contract"
  let
    txOpts = defaultTTCTxOpts appData #
      _from ?~ appData.primaryAccount
    action = TTC.reset txOpts
  Tuple _ e <- takeEvent (Proxy @TTC.PhaseChanged) appData.ttc action
  Console.log $ "Phase changed to " <> show e <> " (0 == Submit Phase)"

defaultTokenTxOpts :: AppData -> TransactionOptions NoPay
defaultTokenTxOpts appData =
  defaultTransactionOptions
    # _gas ?~ bigGasLimit
    # _to ?~ appData.token

defaultTTCTxOpts :: AppData -> TransactionOptions NoPay
defaultTTCTxOpts appData =
  defaultTransactionOptions
    # _gas ?~ bigGasLimit
    # _to ?~ appData.ttc

unsafeToUInt :: Int -> UIntN 256
unsafeToUInt n =
  unsafePartial $ fromJust $ uIntNFromBigNumber (Proxy @256) $ fromInt n

formatAddress :: Address -> String
formatAddress a = unHex (HexString.takeBytes 4 (unAddress a))

bigGasLimit :: BigNumber
bigGasLimit = fromInt 5000000
