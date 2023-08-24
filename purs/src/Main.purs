module Main where

import Prelude

import Chanterelle.Test (assertWeb3)
import Contracts.TTCTrading as TTC
import Contracts.Token as Token
import Control.Parallel (parTraverse)
import Data.Array (length, (!!), (..))
import Data.Either (Either(..), either)
import Data.Homogeneous.Record (fromHomogeneous, homogeneous)
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (for, for_, sequence, traverse_)
import Effect (Effect)
import Effect.Aff (Aff, error, joinFiber, runAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Console (log)
import Effect.Random (randomInt)
import Matrix (formatMatrix)
import Network.Ethereum.Core.Signatures (nullAddress)
import Network.Ethereum.Types (Address, HexString, embed, mkAddress, mkHexString)
import Network.Ethereum.Web3 (ChainCursor(..), DLProxy(..), EventAction(..), Provider, TransactionOptions, UIntN, Web3, _from, _gas, _to, defaultTransactionOptions, eventFilter, forkWeb3, httpProvider, uIntNFromBigNumber)
import Network.Ethereum.Web3.Api (eth_getAccounts)
import Network.Ethereum.Web3.Contract.Events (pollEvent')
import Network.Ethereum.Web3.Solidity (unVector)
import Network.Ethereum.Web3.Solidity.Sizes (S256)
import Network.Ethereum.Web3.Types (NoPay)
import Node.Process (lookupEnv)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = do
  runAff_ (liftEffect <<< log <<< show) $ do
    appData <- mkAppData
    t <- buyTokens appData
    Console.log $ show t
    _ <- transferTokensToTTC appData t
    Console.log "Transfered tokens to TTC"
    _ <- closeSubmissions appData
    Console.log "Sealed the submissions"
    let ranking = ranks t
    Console.log $ show ranking
    _ <- rankTokens appData ranking
    Console.log "Ranked the tokens"
    _ <- verifyPreferences appData ranking
    Console.log "Verified the preferences in contract"
    _ <- closeRankings appData
    Console.log "Emitted Data to Relay"
    _ <- awaitTTCResult appData
    Console.log "Done"

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

type TokenId = UIntN S256

mkAppData :: Aff AppData
mkAppData = do
  provider <- liftEffect $ httpProvider "http://localhost:8545"
  mtoken <- liftEffect $ lookupEnv "TOKEN_ADDRESS"
  mttc <- liftEffect $ lookupEnv "TTC_ADDRESS"
  let
    { token, ttc } = unsafePartial $ fromJust do
      token <- mtoken >>= mkHexString >>= mkAddress
      ttc <- mttc >>= mkHexString >>= mkAddress
      pure { token, ttc }
  users <- assertWeb3 provider getAccounts
  let primaryAccount = users.user1
  pure { provider, token, ttc, users, primaryAccount }
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

awaitTransfer
  :: AppData
  -> Token.Transfer
  -> Web3 Unit
awaitTransfer appData transfer =
  let
    mintMonitor = \e ->
      if e == transfer then pure TerminateEvent
      else pure ContinueEvent
    mintFilter = eventFilter (Proxy :: Proxy Token.Transfer) appData.token
  in
    void $ pollEvent' { f: mintFilter } { f: mintMonitor }

buyTokens
  :: AppData
  -> Aff (Users { user :: Address, tokenId :: TokenId })
buyTokens appData = do
  let users = homogeneous appData.users
  tokenIds <- for users $ \user -> do
    tokenId <- unsafeToUInt <$> liftEffect (randomInt 1 1000000)
    pure { user, tokenId }
  mintFibers <-
    let
      f { user, tokenId } = forkWeb3 appData.provider $
        awaitTransfer appData (Token.Transfer { to: user, from: nullAddress, tokenId })
    in
      parTraverse f tokenIds
  _ <- assertWeb3 appData.provider $ for tokenIds $ \{ user, tokenId } ->
    let
      txOpts = defaultTokenTxOpts appData #
        _from ?~ appData.primaryAccount
    in
      Token.mintToken txOpts { to: user, tokenId }
  res <- parTraverse joinFiber mintFibers
  case sequence res of
    Left err -> throwError $ error $ "Failed to find tokens: " <> show err
    Right _ -> pure $ fromHomogeneous tokenIds

transferTokensToTTC
  :: AppData
  -> Users { user :: Address, tokenId :: TokenId }
  -> Aff Unit
transferTokensToTTC appData _users = do
  let users = homogeneous _users
  transferFibers <-
    let
      f { user, tokenId } = forkWeb3 appData.provider $
        awaitTransfer appData (Token.Transfer { from: user, to: appData.ttc, tokenId })
    in
      parTraverse f users
  _ <- assertWeb3 appData.provider $
    let
      f { user, tokenId } = do
        let
          tokenOpts = defaultTokenTxOpts appData #
            _from ?~ user
        void $ Token.approve tokenOpts { to: appData.ttc, tokenId }
        let
          ttcOpts = defaultTTCTxOpts appData #
            _from ?~ user
        TTC.submitToken ttcOpts { _tokenId: tokenId }
    in
      parTraverse f users
  res <- parTraverse joinFiber transferFibers
  case sequence res of
    Left err -> throwError $ error $ "Failed to find token all token transfers: " <> show err
    Right _ -> pure unit

closeSubmissions :: AppData -> Aff HexString
closeSubmissions appData = assertWeb3 appData.provider $
  let
    txOpts = defaultTTCTxOpts appData #
      _from ?~ appData.primaryAccount
  in
    TTC.sealTokensAndStartRanking txOpts

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

rankTokens
  :: forall r
   . AppData
  -> Users { user :: Address, prefs :: Array TokenId | r }
  -> Aff Unit
rankTokens appData _users = do
  let users = homogeneous _users
  let
    f { user, prefs } = assertWeb3 appData.provider $
      let
        txOpts = defaultTTCTxOpts appData #
          _from ?~ user
      in
        TTC.submitPreferences txOpts { _preferenceList: prefs }
  _ <- parTraverse f users
  pure unit
--where

verifyPreferences
  :: AppData
  -> Users { user :: Address, tokenId :: TokenId, prefs :: Array TokenId }
  -> Aff Unit
verifyPreferences appData _users = do
  let users = homogeneous _users
  let
    f { user, prefs } = assertWeb3 appData.provider $ do
      Console.log $ "Finding ranking for " <> show user
      let
        txOpts = defaultTTCTxOpts appData
        ownerIdxs = (0 .. 5)
      for_ ownerIdxs $ \ix -> do
        eowner <- TTC.ownersArray txOpts Latest (unsafeToUInt ix)
        owner <- either (throwError <<< error <<< show) pure eowner
        when (owner == user) $ do
          Console.log $ "owner at index " <> show ix <> " is " <> show eowner
          let prefIdxs = (0 .. (length prefs - 1))
          for_ prefIdxs \prefIndex -> do
            Console.log $ "Checking preference at index " <> (show ix) <> ", " <> show prefIndex
            pref <- TTC.preferenceListsArray txOpts Latest (unsafeToUInt ix) (unsafeToUInt prefIndex)
            let trueVal = prefs !! prefIndex
            unless (Just pref == map Right trueVal)
              $ throwError
              $ error
              $ "wanted " <> show trueVal <> " but got " <> show pref
  traverse_ f users

closeRankings
  :: AppData
  -> Aff Unit
closeRankings appData = do
  f <- forkWeb3 appData.provider awaitTokenDetailsEmitted
  _ <- assertWeb3 appData.provider $
    let
      txOpts = defaultTTCTxOpts appData #
        _from ?~ appData.primaryAccount
    in
      TTC.lockRankingAndExecuteTTC txOpts
  res <- joinFiber f
  case res of
    Left err -> throwError $ error $ "Failed to find DetailsEmitted: " <> show err
    Right _ -> pure unit

  where
  awaitTokenDetailsEmitted =
    let
      tdeMonitor = \(TTC.TokenDetailsEmitted { tokenIds, preferenceLists }) -> do
        Console.log $
          "Corresponds to preference matrix: \n" <>
            formatMatrix { header: unVector tokenIds, matrix: unVector preferenceLists }
        pure TerminateEvent
      tdeFilter = eventFilter (Proxy :: Proxy TTC.TokenDetailsEmitted) appData.ttc
    in
      void $ pollEvent' { f: tdeFilter } { f: tdeMonitor }

awaitTTCResult
  :: AppData
  -> Aff Unit
awaitTTCResult appData =
  let
    monitor = \e@(TTC.TTCResult {}) -> do
      Console.log $ show e
      pure TerminateEvent
    filter = eventFilter (Proxy :: Proxy TTC.TTCResult) appData.ttc
  in
    void $ assertWeb3 appData.provider $ pollEvent' { f: filter } { f: monitor }

defaultTokenTxOpts :: AppData -> TransactionOptions NoPay
defaultTokenTxOpts appData =
  defaultTransactionOptions
    # _gas ?~ embed 1000000
    # _to ?~ appData.token

defaultTTCTxOpts :: AppData -> TransactionOptions NoPay
defaultTTCTxOpts appData =
  defaultTransactionOptions
    # _gas ?~ embed 1000000
    # _to ?~ appData.ttc

unsafeToUInt :: Int -> UIntN S256
unsafeToUInt n =
  unsafePartial $ fromJust $ uIntNFromBigNumber (DLProxy :: DLProxy S256) $ embed n
