module Contracts.TTCTrading where

import Prelude

import Data.Either (Either)
import Data.Eq.Generic (genericEq)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Lens (set)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Network.Ethereum.Web3 (Vector, Web3, class EventFilter, _address, _topics, call, deployContract, sendTx)
import Network.Ethereum.Web3.Solidity (ByteString, BytesN, Tuple2, Tuple3, UIntN, Tuple0(..), Tuple1(..), class IndexedEvent, fromRecord, unTuple1)
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)

type FnConstructorInput = Tagged Void
  ( Tuple3 (Tagged "_token" (Identity Address)) (Tagged "bonsaiRelay" (Identity Address))
      (Tagged "_imageId" (Identity (BytesN 32)))
  )

type FnConstructorOutput = Tuple0

constructor
  :: TransactionOptions NoPay
  -> HexString
  -> { _token :: Address, bonsaiRelay :: Address, _imageId :: BytesN 32 }
  -> Web3 HexString
constructor bytecode txOpts x = deployContract bytecode txOpts
  (tagged (fromRecord x) :: FnConstructorInput)

newtype PhaseChanged = PhaseChanged { newPhase :: UIntN 8 }

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
        ]
    )

instance IndexedEvent Tuple0 (Tuple1 (Tagged "newPhase" (Identity (UIntN 8)))) PhaseChanged where
  isAnonymous _ = false

newtype TTCResult = TTCResult { result :: Array (Array (UIntN 256)) }

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
        ]
    )

instance
  IndexedEvent Tuple0 (Tuple1 (Tagged "result" (Identity (Array (Array (UIntN 256)))))) TTCResult where
  isAnonymous _ = false

newtype TokenDetailsEmitted = TokenDetailsEmitted
  { tokenIds :: Vector 6 (UIntN 256), preferenceLists :: Vector 6 (Array (UIntN 256)) }

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
        ]
    )

instance
  IndexedEvent Tuple0
    ( Tuple2 (Tagged "tokenIds" (Identity (Vector 6 (UIntN 256))))
        (Tagged "preferenceLists" (Identity (Vector 6 (Array (UIntN 256)))))
    )
    TokenDetailsEmitted where
  isAnonymous _ = false

type FnMAX_PARTICIPANTSInput = Tagged "MAX_PARTICIPANTS()" Tuple0
type FnMAX_PARTICIPANTSOutput = Tuple1 (UIntN 8)

mAX_PARTICIPANTS :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN 8))
mAX_PARTICIPANTS txOpts chainCursor = map unTuple1 <$> call txOpts chainCursor
  (tagged Tuple0 :: FnMAX_PARTICIPANTSInput)

type FnBonsaiRelayInput = Tagged "bonsaiRelay()" Tuple0
type FnBonsaiRelayOutput = Tuple1 Address

bonsaiRelay :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
bonsaiRelay txOpts chainCursor = map unTuple1 <$> call txOpts chainCursor
  (tagged Tuple0 :: FnBonsaiRelayInput)

type FnLockRankingAndExecuteTTCInput = Tagged "lockRankingAndExecuteTTC()" Tuple0
type FnLockRankingAndExecuteTTCOutput = Tuple0

lockRankingAndExecuteTTC :: TransactionOptions NoPay -> Web3 HexString
lockRankingAndExecuteTTC txOpts = sendTx txOpts (tagged Tuple0 :: FnLockRankingAndExecuteTTCInput)

type FnOwnersArrayInput = Tagged "ownersArray(uint256)" (Tuple1 (UIntN 256))
type FnOwnersArrayOutput = Tuple1 Address

ownersArray
  :: TransactionOptions NoPay -> ChainCursor -> UIntN 256 -> Web3 (Either CallError Address)
ownersArray txOpts chainCursor x = map unTuple1 <$> call txOpts chainCursor
  (tagged (Tuple1 x) :: FnOwnersArrayInput)

type FnPhaseInput = Tagged "phase()" Tuple0
type FnPhaseOutput = Tuple1 (UIntN 8)

phase :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN 8))
phase txOpts chainCursor = map unTuple1 <$> call txOpts chainCursor (tagged Tuple0 :: FnPhaseInput)

type FnPreferenceListsArrayInput = Tagged "preferenceListsArray(uint256,uint256)"
  (Tuple2 (Tagged "_1" (Identity (UIntN 256))) (Tagged "_2" (Identity (UIntN 256))))

type FnPreferenceListsArrayOutput = Tuple1 (UIntN 256)

preferenceListsArray
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { _1 :: UIntN 256, _2 :: UIntN 256 }
  -> Web3 (Either CallError (UIntN 256))
preferenceListsArray txOpts chainCursor x = map unTuple1 <$> call txOpts chainCursor
  (tagged (fromRecord x) :: FnPreferenceListsArrayInput)

type FnResetInput = Tagged "reset()" Tuple0
type FnResetOutput = Tuple0

reset :: TransactionOptions NoPay -> Web3 HexString
reset txOpts = sendTx txOpts (tagged Tuple0 :: FnResetInput)

type FnRetrieveTokenInput = Tagged "retrieveToken()" Tuple0
type FnRetrieveTokenOutput = Tuple0

retrieveToken :: TransactionOptions NoPay -> Web3 HexString
retrieveToken txOpts = sendTx txOpts (tagged Tuple0 :: FnRetrieveTokenInput)

type FnSealTokensAndStartRankingInput = Tagged "sealTokensAndStartRanking()" Tuple0
type FnSealTokensAndStartRankingOutput = Tuple0

sealTokensAndStartRanking :: TransactionOptions NoPay -> Web3 HexString
sealTokensAndStartRanking txOpts = sendTx txOpts
  (tagged Tuple0 :: FnSealTokensAndStartRankingInput)

type FnStoreResultInput = Tagged "storeResult(uint256[][])"
  (Tuple1 (Tagged "result" (Identity (Array (Array (UIntN 256))))))

type FnStoreResultOutput = Tuple1 ByteString

storeResult
  :: TransactionOptions NoPay -> { result :: Array (Array (UIntN 256)) } -> Web3 HexString
storeResult txOpts x = sendTx txOpts (tagged (fromRecord x) :: FnStoreResultInput)

type FnSubmissionCounterInput = Tagged "submissionCounter()" Tuple0
type FnSubmissionCounterOutput = Tuple1 (UIntN 8)

submissionCounter :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN 8))
submissionCounter txOpts chainCursor = map unTuple1 <$> call txOpts chainCursor
  (tagged Tuple0 :: FnSubmissionCounterInput)

type FnSubmitPreferencesInput = Tagged "submitPreferences(uint256[])"
  (Tuple1 (Tagged "_preferenceList" (Identity (Array (UIntN 256)))))

type FnSubmitPreferencesOutput = Tuple0

submitPreferences
  :: TransactionOptions NoPay -> { _preferenceList :: Array (UIntN 256) } -> Web3 HexString
submitPreferences txOpts x = sendTx txOpts (tagged (fromRecord x) :: FnSubmitPreferencesInput)

type FnSubmitTokenInput = Tagged "submitToken(uint256)"
  (Tuple1 (Tagged "_tokenId" (Identity (UIntN 256))))

type FnSubmitTokenOutput = Tuple0

submitToken :: TransactionOptions NoPay -> { _tokenId :: UIntN 256 } -> Web3 HexString
submitToken txOpts x = sendTx txOpts (tagged (fromRecord x) :: FnSubmitTokenInput)

type FnTokenInput = Tagged "token()" Tuple0
type FnTokenOutput = Tuple1 Address

token :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
token txOpts chainCursor = map unTuple1 <$> call txOpts chainCursor (tagged Tuple0 :: FnTokenInput)

type FnTokenIdsArrayInput = Tagged "tokenIdsArray(uint256)" (Tuple1 (UIntN 256))
type FnTokenIdsArrayOutput = Tuple1 (UIntN 256)

tokenIdsArray
  :: TransactionOptions NoPay -> ChainCursor -> UIntN 256 -> Web3 (Either CallError (UIntN 256))
tokenIdsArray txOpts chainCursor x = map unTuple1 <$> call txOpts chainCursor
  (tagged (Tuple1 x) :: FnTokenIdsArrayInput)

type FnTtcImageIdInput = Tagged "ttcImageId()" Tuple0
type FnTtcImageIdOutput = Tuple1 (BytesN 32)

ttcImageId :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (BytesN 32))
ttcImageId txOpts chainCursor = map unTuple1 <$> call txOpts chainCursor
  (tagged Tuple0 :: FnTtcImageIdInput)
