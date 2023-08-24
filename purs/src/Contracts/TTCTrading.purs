module Contracts.TTCTrading where

import Prelude

import Data.Either (Either)
import Data.Eq.Generic (genericEq)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Lens (set)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Network.Ethereum.Web3 (Vector, class EventFilter, _address, _topics, call, deployContract, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (BytesN, D2, D3, D5, D6, D8, DOne, UIntN, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy)

type ConstructorFn = Tagged (Proxy Void)
  ( Tuple3 (Tagged (Proxy "_token") Address) (Tagged (Proxy "bonsaiRelay") Address)
      (Tagged (Proxy "_imageId") (BytesN (D3 :& DOne D2)))
  )

constructor
  :: TransactionOptions NoPay
  -> HexString
  -> { _token :: Address, bonsaiRelay :: Address, _imageId :: BytesN (D3 :& DOne D2) }
  -> Web3 HexString
constructor x1 x2 x3 = uncurryFields x3 $ constructor' x1 x2
  where
  constructor'
    :: TransactionOptions NoPay
    -> HexString
    -> Tagged (Proxy "_token") Address
    -> Tagged (Proxy "bonsaiRelay") Address
    -> Tagged (Proxy "_imageId") (BytesN (D3 :& DOne D2))
    -> Web3 HexString
  constructor' _x1 _x2 _x3 _x4 _x5 = deployContract _x1 _x2
    (tagged $ Tuple3 _x3 _x4 _x5 :: ConstructorFn)

newtype PhaseChanged = PhaseChanged { newPhase :: UIntN (DOne D8) }

derive instance Newtype PhaseChanged _
derive instance Generic PhaseChanged _
instance Show PhaseChanged where
  show = genericShow

instance Eq PhaseChanged where
  eq = genericEq

instance EventFilter PhaseChanged where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "a6dcc92f45df25789d5639b7a0c97ba1edf3bb1c0b5dd3376fd96a0db87c4642"
        , Nothing
        , Nothing
        ]
    )

instance IndexedEvent Tuple0 (Tuple1 (Tagged (Proxy "newPhase") (UIntN (DOne D8)))) PhaseChanged where
  isAnonymous _ = false

newtype TTCResult = TTCResult { result :: Array (Array (UIntN (D2 :& D5 :& DOne D6))) }

derive instance Newtype TTCResult _
derive instance Generic TTCResult _
instance Show TTCResult where
  show = genericShow

instance Eq TTCResult where
  eq = genericEq

instance EventFilter TTCResult where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "e9e3de945b165f61e7b722e151b49faba845b935ee3880b74cdfa09f3a0421de"
        , Nothing
        , Nothing
        ]
    )

instance
  IndexedEvent Tuple0
    (Tuple1 (Tagged (Proxy "result") (Array (Array (UIntN (D2 :& D5 :& DOne D6))))))
    TTCResult where
  isAnonymous _ = false

newtype TokenDetailsEmitted = TokenDetailsEmitted
  { tokenIds :: Vector (DOne D6) (UIntN (D2 :& D5 :& DOne D6))
  , preferenceLists :: Vector (DOne D6) (Array (UIntN (D2 :& D5 :& DOne D6)))
  }

derive instance Newtype TokenDetailsEmitted _
derive instance Generic TokenDetailsEmitted _
instance Show TokenDetailsEmitted where
  show = genericShow

instance Eq TokenDetailsEmitted where
  eq = genericEq

instance EventFilter TokenDetailsEmitted where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "e58780ba0390d72735fcc7f1706d8541c5cf7cfc291290971765006618f619fb"
        , Nothing
        , Nothing
        ]
    )

instance
  IndexedEvent Tuple0
    ( Tuple2 (Tagged (Proxy "tokenIds") (Vector (DOne D6) (UIntN (D2 :& D5 :& DOne D6))))
        (Tagged (Proxy "preferenceLists") (Vector (DOne D6) (Array (UIntN (D2 :& D5 :& DOne D6)))))
    )
    TokenDetailsEmitted where
  isAnonymous _ = false

type MAX_PARTICIPANTSFn = Tagged (Proxy "MAX_PARTICIPANTS()") Tuple0

mAX_PARTICIPANTS
  :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (DOne D8)))
mAX_PARTICIPANTS x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: MAX_PARTICIPANTSFn)

type BonsaiRelayFn = Tagged (Proxy "bonsaiRelay()") Tuple0

bonsaiRelay :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
bonsaiRelay x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: BonsaiRelayFn)

type LockRankingAndExecuteTTCFn = Tagged (Proxy "lockRankingAndExecuteTTC()") Tuple0

lockRankingAndExecuteTTC :: TransactionOptions NoPay -> Web3 HexString
lockRankingAndExecuteTTC x1 = sendTx x1 (tagged Tuple0 :: LockRankingAndExecuteTTCFn)

type OwnersArrayFn = Tagged (Proxy "ownersArray(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

ownersArray
  :: TransactionOptions NoPay
  -> ChainCursor
  -> UIntN (D2 :& D5 :& DOne D6)
  -> Web3 (Either CallError Address)
ownersArray x1 x2 x3 = ownersArray' x1 x2 x3
  where
  ownersArray'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> UIntN (D2 :& D5 :& DOne D6)
    -> Web3 (Either CallError Address)
  ownersArray' _x1 _x2 _x3 = map unTuple1 <$> call _x1 _x2 (tagged $ Tuple1 _x3 :: OwnersArrayFn)

type PhaseFn = Tagged (Proxy "phase()") Tuple0

phase :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (DOne D8)))
phase x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: PhaseFn)

type PreferenceListsArrayFn = Tagged (Proxy "preferenceListsArray(uint256,uint256)")
  (Tuple2 (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)))

preferenceListsArray
  :: TransactionOptions NoPay
  -> ChainCursor
  -> UIntN (D2 :& D5 :& DOne D6)
  -> UIntN (D2 :& D5 :& DOne D6)
  -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
preferenceListsArray x1 x2 x3 x4 = preferenceListsArray' x1 x2 x3 x4
  where
  preferenceListsArray'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> UIntN (D2 :& D5 :& DOne D6)
    -> UIntN (D2 :& D5 :& DOne D6)
    -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
  preferenceListsArray' _x1 _x2 _x3 _x4 = map unTuple1 <$> call _x1 _x2
    (tagged $ Tuple2 _x3 _x4 :: PreferenceListsArrayFn)

type ResetFn = Tagged (Proxy "reset()") Tuple0

reset :: TransactionOptions NoPay -> Web3 HexString
reset x1 = sendTx x1 (tagged Tuple0 :: ResetFn)

type RetrieveTokenFn = Tagged (Proxy "retrieveToken()") Tuple0

retrieveToken :: TransactionOptions NoPay -> Web3 HexString
retrieveToken x1 = sendTx x1 (tagged Tuple0 :: RetrieveTokenFn)

type SealTokensAndStartRankingFn = Tagged (Proxy "sealTokensAndStartRanking()") Tuple0

sealTokensAndStartRanking :: TransactionOptions NoPay -> Web3 HexString
sealTokensAndStartRanking x1 = sendTx x1 (tagged Tuple0 :: SealTokensAndStartRankingFn)

type StoreResultFn = Tagged (Proxy "storeResult(uint256[][])")
  (Tuple1 (Tagged (Proxy "result") (Array (Array (UIntN (D2 :& D5 :& DOne D6))))))

storeResult
  :: TransactionOptions NoPay
  -> { result :: Array (Array (UIntN (D2 :& D5 :& DOne D6))) }
  -> Web3 HexString
storeResult x1 x2 = uncurryFields x2 $ storeResult' x1
  where
  storeResult'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "result") (Array (Array (UIntN (D2 :& D5 :& DOne D6))))
    -> Web3 HexString
  storeResult' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: StoreResultFn)

type SubmissionCounterFn = Tagged (Proxy "submissionCounter()") Tuple0

submissionCounter
  :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (DOne D8)))
submissionCounter x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: SubmissionCounterFn)

type SubmitPreferencesFn = Tagged (Proxy "submitPreferences(uint256[])")
  (Tuple1 (Tagged (Proxy "_preferenceList") (Array (UIntN (D2 :& D5 :& DOne D6)))))

submitPreferences
  :: TransactionOptions NoPay
  -> { _preferenceList :: Array (UIntN (D2 :& D5 :& DOne D6)) }
  -> Web3 HexString
submitPreferences x1 x2 = uncurryFields x2 $ submitPreferences' x1
  where
  submitPreferences'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "_preferenceList") (Array (UIntN (D2 :& D5 :& DOne D6)))
    -> Web3 HexString
  submitPreferences' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: SubmitPreferencesFn)

type SubmitTokenFn = Tagged (Proxy "submitToken(uint256)")
  (Tuple1 (Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

submitToken
  :: TransactionOptions NoPay -> { _tokenId :: UIntN (D2 :& D5 :& DOne D6) } -> Web3 HexString
submitToken x1 x2 = uncurryFields x2 $ submitToken' x1
  where
  submitToken'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  submitToken' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: SubmitTokenFn)

type TokenFn = Tagged (Proxy "token()") Tuple0

token :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
token x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: TokenFn)

type TokenIdsArrayFn = Tagged (Proxy "tokenIdsArray(uint256)")
  (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

tokenIdsArray
  :: TransactionOptions NoPay
  -> ChainCursor
  -> UIntN (D2 :& D5 :& DOne D6)
  -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
tokenIdsArray x1 x2 x3 = tokenIdsArray' x1 x2 x3
  where
  tokenIdsArray'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> UIntN (D2 :& D5 :& DOne D6)
    -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
  tokenIdsArray' _x1 _x2 _x3 = map unTuple1 <$> call _x1 _x2
    (tagged $ Tuple1 _x3 :: TokenIdsArrayFn)

type TtcImageIdFn = Tagged (Proxy "ttcImageId()") Tuple0

ttcImageId
  :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (BytesN (D3 :& DOne D2)))
ttcImageId x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: TtcImageIdFn)
