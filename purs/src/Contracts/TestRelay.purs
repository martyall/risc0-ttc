module Contracts.TestRelay where

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
import Network.Ethereum.Web3 (Web3, class EventFilter, _address, _topics, call, deployContract, sendTx)
import Network.Ethereum.Web3.Solidity (ByteString, BytesN, Tuple0, Tuple1, Tuple2, Tuple3, Tuple4, Tuple5, Tuple6, UIntN, class IndexedEvent, fromRecord, toRecord, unTuple1)
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)

type FnConstructorInput = Tagged Void (Tuple1 (Tagged "chainId" (Identity (UIntN 256))))
type FnConstructorOutput = Tuple0

constructor :: TransactionOptions NoPay -> HexString -> { chainId :: UIntN 256 } -> Web3 HexString
constructor bytecode txOpts x = deployContract bytecode txOpts
  (tagged (fromRecord x) :: FnConstructorInput)

newtype CallbackRequest = CallbackRequest
  { account :: Address
  , imageId :: BytesN 32
  , input :: ByteString
  , callbackContract :: Address
  , functionSelector :: BytesN 4
  , gasLimit :: UIntN 64
  }

derive instance Newtype CallbackRequest _
derive instance Generic CallbackRequest _
instance Show CallbackRequest where
  show = genericShow

instance Eq CallbackRequest where
  eq = genericEq

instance EventFilter CallbackRequest where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "105b7c9dbbc865638cd7873341700bcf4c62dfc9a4f221425b56fde3408f6a85"
        ]
    )

instance
  IndexedEvent Tuple0
    ( Tuple6 (Tagged "account" (Identity Address)) (Tagged "imageId" (Identity (BytesN 32)))
        (Tagged "input" (Identity ByteString))
        (Tagged "callbackContract" (Identity Address))
        (Tagged "functionSelector" (Identity (BytesN 4)))
        (Tagged "gasLimit" (Identity (UIntN 64)))
    )
    CallbackRequest where
  isAnonymous _ = false

type FnCallbackIsAuthorizedInput = Tagged "callbackIsAuthorized(bytes32,bytes,(bytes,bytes32))"
  ( Tuple3 (Tagged "_1" (Identity (BytesN 32))) (Tagged "_2" (Identity ByteString))
      ( Tagged "auth"
          ( Tuple2 (Tagged "seal" (Identity ByteString))
              (Tagged "postStateDigest" (Identity (BytesN 32)))
          )
      )
  )

type FnCallbackIsAuthorizedOutput = Tuple1 Boolean

callbackIsAuthorized
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { _1 :: BytesN 32
     , _2 :: ByteString
     , auth :: { seal :: ByteString, postStateDigest :: BytesN 32 }
     }
  -> Web3 (Either CallError Boolean)
callbackIsAuthorized txOpts chainCursor x = map unTuple1 <$> call txOpts chainCursor
  (tagged (fromRecord x) :: FnCallbackIsAuthorizedInput)

type FnInvokeCallbackInput = Tagged "invokeCallback(((bytes,bytes32),address,bytes,uint64))"
  ( Tuple1
      ( Tagged "callback"
          ( Tuple4
              ( Tagged "auth"
                  ( Tuple2 (Tagged "seal" (Identity ByteString))
                      (Tagged "postStateDigest" (Identity (BytesN 32)))
                  )
              )
              (Tagged "callbackContract" (Identity Address))
              (Tagged "payload" (Identity ByteString))
              (Tagged "gasLimit" (Identity (UIntN 64)))
          )
      )
  )

type FnInvokeCallbackOutput = Tuple0

invokeCallback
  :: TransactionOptions NoPay
  -> { callback ::
         { auth :: { seal :: ByteString, postStateDigest :: BytesN 32 }
         , callbackContract :: Address
         , payload :: ByteString
         , gasLimit :: UIntN 64
         }
     }
  -> Web3 HexString
invokeCallback txOpts x = sendTx txOpts (tagged (fromRecord x) :: FnInvokeCallbackInput)

type FnInvokeCallbacksInput = Tagged "invokeCallbacks(((bytes,bytes32),address,bytes,uint64)[])"
  ( Tuple1
      ( Tagged "callbacks"
          ( Array
              ( Tuple4
                  ( Tagged "auth"
                      ( Tuple2 (Tagged "seal" (Identity ByteString))
                          (Tagged "postStateDigest" (Identity (BytesN 32)))
                      )
                  )
                  (Tagged "callbackContract" (Identity Address))
                  (Tagged "payload" (Identity ByteString))
                  (Tagged "gasLimit" (Identity (UIntN 64)))
              )
          )
      )
  )

type FnInvokeCallbacksOutput = Tuple1 (Tagged "invocationResults" (Identity (Array Boolean)))

invokeCallbacks
  :: TransactionOptions NoPay
  -> { callbacks ::
         Array
           { auth :: { seal :: ByteString, postStateDigest :: BytesN 32 }
           , callbackContract :: Address
           , payload :: ByteString
           , gasLimit :: UIntN 64
           }
     }
  -> Web3 HexString
invokeCallbacks txOpts x = sendTx txOpts (tagged (fromRecord x) :: FnInvokeCallbacksInput)

type FnParsePayloadInput = Tagged "parsePayload(bytes)"
  (Tuple1 (Tagged "payload" (Identity ByteString)))

type FnParsePayloadOutput = Tuple2 (Tagged "_1" (Identity (BytesN 32)))
  (Tagged "_2" (Identity ByteString))

parsePayload
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { payload :: ByteString }
  -> Web3 (Either CallError { _1 :: BytesN 32, _2 :: ByteString })
parsePayload txOpts chainCursor x = do
  eRes :: Either _ FnParsePayloadOutput <- call txOpts chainCursor
    (tagged (fromRecord x) :: FnParsePayloadInput)
  pure (map toRecord eRes)

type FnRequestCallbackInput = Tagged "requestCallback(bytes32,bytes,address,bytes4,uint64)"
  ( Tuple5 (Tagged "imageId" (Identity (BytesN 32))) (Tagged "input" (Identity ByteString))
      (Tagged "callbackContract" (Identity Address))
      (Tagged "functionSelector" (Identity (BytesN 4)))
      (Tagged "gasLimit" (Identity (UIntN 64)))
  )

type FnRequestCallbackOutput = Tuple0

requestCallback
  :: TransactionOptions NoPay
  -> { imageId :: BytesN 32
     , input :: ByteString
     , callbackContract :: Address
     , functionSelector :: BytesN 4
     , gasLimit :: UIntN 64
     }
  -> Web3 HexString
requestCallback txOpts x = sendTx txOpts (tagged (fromRecord x) :: FnRequestCallbackInput)
