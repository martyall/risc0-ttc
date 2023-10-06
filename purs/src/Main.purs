module Main
  ( AppData
  , TokenId
  , Users
  , awaitTTCResult
  , awaitTransfer
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

import Chanterelle.Test (assertWeb3)
import Contracts.TTCTrading as TTC
import Contracts.Token as Token
import Control.Monad.Reader (ask)
import Control.Parallel (parTraverse)
import Cycle (applyCycles, mkCycle)
import Data.Array (length, (!!), (..))
import Data.Either (Either(..), either)
import Data.Homogeneous.Record (fromHomogeneous, homogeneous)
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (for, for_, sequence, traverse, traverse_)
import Effect (Effect)
import Effect.Aff (Aff, error, joinFiber, launchAff_, throwError)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Effect.Random (randomInt)
import Matrix (formatMatrix)
import Network.Ethereum.Core.HexString (unHex)
import Network.Ethereum.Core.HexString as HexString
import Network.Ethereum.Core.Signatures (nullAddress, unAddress)
import Network.Ethereum.Types (Address, HexString, fromInt, mkAddress, mkHexString)
import Network.Ethereum.Web3 (ChainCursor(..), Change(..), EventAction(..), Provider, TransactionOptions, UIntN, Web3, _from, _gas, _to, defaultTransactionOptions, eventFilter, forkWeb3, httpProvider, uIntNFromBigNumber)
import Network.Ethereum.Web3.Api (eth_getAccounts)
import Network.Ethereum.Web3.Contract.Events (pollEvent')
import Network.Ethereum.Web3.Solidity (unVector, Tuple2(..))
import Network.Ethereum.Web3.Types (NoPay)
import Node.Process (lookupEnv)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = launchAff_ do
  appData <- mkAppData
  t <- buyTokens appData
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
  -> { from :: Address, to :: Address }
  -> Web3 TokenId
awaitTransfer appData { from, to } =
  let
    mintMonitor var = \(Token.Transfer t) ->
      if t.from == from && t.to == to then do
        liftAff $ AVar.put t.tokenId var
        pure TerminateEvent
      else
        pure ContinueEvent
    mintFilter = eventFilter (Proxy :: Proxy Token.Transfer) appData.token
  in
    do
      var <- liftAff $ AVar.empty
      void $ pollEvent' { f: mintFilter } { f: mintMonitor var }
      liftAff $ AVar.take var

buyTokens
  :: AppData
  -> Aff (Users { user :: Address, tokenId :: TokenId })
buyTokens appData = do
  let users = homogeneous appData.users
  tokenIds <- for users $ \user -> do
    tokenId <- unsafeToUInt <$> liftEffect (randomInt 1 10000)
    pure { user, tokenId }
  mintFibers <-
    let
      f { user } = forkWeb3 appData.provider $
        awaitTransfer appData { to: user, from: nullAddress }
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
      f { user } = forkWeb3 appData.provider $
        awaitTransfer appData { from: user, to: appData.ttc }
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
            pref <- TTC.preferenceListsArray txOpts Latest $ Tuple2 (unsafeToUInt ix) (unsafeToUInt prefIndex)
            let trueVal = prefs !! prefIndex
            unless (Just pref == map Right trueVal)
              $ throwError
              $ error
              $ "For index " <> show ix <> ", " <> show prefIndex
                  <> "wanted "
                  <> show trueVal
                  <> " but got "
                  <> show pref
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
        Change c <- ask
        Console.log $ "TokenDetailsEmittedEvent: \n" <> unHex c.data
        Console.info $
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
  let users = homogeneous _users
  transferFibers <-
    let
      f { user, tokenId } = forkWeb3 appData.provider $ do
        trade <- awaitTransfer appData { from: appData.ttc, to: user }
        pure { user, tokenId, trade }
    in
      parTraverse f users
  _ <- assertWeb3 appData.provider $
    let
      f { user } = do
        let
          ttcOpts = defaultTTCTxOpts appData #
            _from ?~ user
        TTC.retrieveToken ttcOpts
    in
      parTraverse f users
  res <- parTraverse joinFiber transferFibers
  case sequence res of
    Left err -> throwError $ error $ "Failed to find token all token transfers: " <> show err
    Right a -> pure $ fromHomogeneous a

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
resetContract appData = do
  f <- forkWeb3 appData.provider awaitPhaseChange
  _ <- assertWeb3 appData.provider $
    let
      txOpts = defaultTTCTxOpts appData #
        _from ?~ appData.primaryAccount
    in
      TTC.reset txOpts
  res <- joinFiber f
  case res of
    Left err -> throwError $ error $ "Failed to reset contract: " <> show err
    Right _ -> pure unit

  where
  awaitPhaseChange =
    let
      monitor = \(TTC.PhaseChanged { newPhase }) -> do
        Console.log $ "Phase changed to " <> show newPhase <> " (0 == Submit Phase)"
        pure TerminateEvent
      filter = eventFilter (Proxy :: Proxy TTC.PhaseChanged) appData.ttc
    in
      void $ pollEvent' { f: filter } { f: monitor }

defaultTokenTxOpts :: AppData -> TransactionOptions NoPay
defaultTokenTxOpts appData =
  defaultTransactionOptions
    # _gas ?~ fromInt 1000000
    # _to ?~ appData.token

defaultTTCTxOpts :: AppData -> TransactionOptions NoPay
defaultTTCTxOpts appData =
  defaultTransactionOptions
    # _gas ?~ fromInt 1000000
    # _to ?~ appData.ttc

unsafeToUInt :: Int -> UIntN 256
unsafeToUInt n =
  unsafePartial $ fromJust $ uIntNFromBigNumber (Proxy @256) $ fromInt n

formatAddress :: Address -> String
formatAddress a = unHex (HexString.takeBytes 4 (unAddress a))
