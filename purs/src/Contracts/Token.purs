module Contracts.Token where

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
import Network.Ethereum.Web3.Solidity (ByteString, BytesN, Tuple1, Tuple2, Tuple3, Tuple4, UIntN, Tuple0(..), class IndexedEvent, fromRecord, unTuple1)
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)

type FnConstructorInput = Tagged Void Tuple0
type FnConstructorOutput = Tuple0

constructor :: TransactionOptions NoPay -> HexString -> Web3 HexString
constructor bytecode txOpts = deployContract bytecode txOpts (tagged Tuple0 :: FnConstructorInput)

newtype Approval = Approval { owner :: Address, approved :: Address, tokenId :: UIntN 256 }

derive instance Newtype Approval _
derive instance Generic Approval _
instance Show Approval where
  show = genericShow

instance Eq Approval where
  eq = genericEq

instance EventFilter Approval where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925"
        , Nothing
        , Nothing
        , Nothing
        ]
    )

instance
  IndexedEvent ( Tuple3 (Tagged "owner" (Identity Address)) (Tagged "approved" (Identity Address))
        (Tagged "tokenId" (Identity (UIntN 256)))
    )
    Tuple0
    Approval where
  isAnonymous _ = false

newtype ApprovalForAll = ApprovalForAll
  { owner :: Address, operator :: Address, approved :: Boolean }

derive instance Newtype ApprovalForAll _
derive instance Generic ApprovalForAll _
instance Show ApprovalForAll where
  show = genericShow

instance Eq ApprovalForAll where
  eq = genericEq

instance EventFilter ApprovalForAll where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "17307eab39ab6107e8899845ad3d59bd9653f200f220920489ca2b5937696c31"
        , Nothing
        , Nothing
        ]
    )

instance
  IndexedEvent (Tuple2 (Tagged "owner" (Identity Address)) (Tagged "operator" (Identity Address)))
    (Tuple1 (Tagged "approved" (Identity Boolean)))
    ApprovalForAll where
  isAnonymous _ = false

newtype Transfer = Transfer { from :: Address, to :: Address, tokenId :: UIntN 256 }

derive instance Newtype Transfer _
derive instance Generic Transfer _
instance Show Transfer where
  show = genericShow

instance Eq Transfer where
  eq = genericEq

instance EventFilter Transfer where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"
        , Nothing
        , Nothing
        , Nothing
        ]
    )

instance
  IndexedEvent ( Tuple3 (Tagged "from" (Identity Address)) (Tagged "to" (Identity Address))
        (Tagged "tokenId" (Identity (UIntN 256)))
    )
    Tuple0
    Transfer where
  isAnonymous _ = false

type FnApproveInput = Tagged "approve(address,uint256)"
  (Tuple2 (Tagged "to" (Identity Address)) (Tagged "tokenId" (Identity (UIntN 256))))

type FnApproveOutput = Tuple0

approve :: TransactionOptions NoPay -> { to :: Address, tokenId :: UIntN 256 } -> Web3 HexString
approve txOpts x = sendTx txOpts (tagged (fromRecord x) :: FnApproveInput)

type FnBalanceOfInput = Tagged "balanceOf(address)" (Tuple1 (Tagged "owner" (Identity Address)))
type FnBalanceOfOutput = Tuple1 (UIntN 256)

balanceOf
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { owner :: Address }
  -> Web3 (Either CallError (UIntN 256))
balanceOf txOpts chainCursor x = map unTuple1 <$> call txOpts chainCursor
  (tagged (fromRecord x) :: FnBalanceOfInput)

type FnGetApprovedInput = Tagged "getApproved(uint256)"
  (Tuple1 (Tagged "tokenId" (Identity (UIntN 256))))

type FnGetApprovedOutput = Tuple1 Address

getApproved
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { tokenId :: UIntN 256 }
  -> Web3 (Either CallError Address)
getApproved txOpts chainCursor x = map unTuple1 <$> call txOpts chainCursor
  (tagged (fromRecord x) :: FnGetApprovedInput)

type FnIsApprovedForAllInput = Tagged "isApprovedForAll(address,address)"
  (Tuple2 (Tagged "owner" (Identity Address)) (Tagged "operator" (Identity Address)))

type FnIsApprovedForAllOutput = Tuple1 Boolean

isApprovedForAll
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { owner :: Address, operator :: Address }
  -> Web3 (Either CallError Boolean)
isApprovedForAll txOpts chainCursor x = map unTuple1 <$> call txOpts chainCursor
  (tagged (fromRecord x) :: FnIsApprovedForAllInput)

type FnMintTokenInput = Tagged "mintToken(address,uint256)"
  (Tuple2 (Tagged "to" (Identity Address)) (Tagged "tokenId" (Identity (UIntN 256))))

type FnMintTokenOutput = Tuple0

mintToken :: TransactionOptions NoPay -> { to :: Address, tokenId :: UIntN 256 } -> Web3 HexString
mintToken txOpts x = sendTx txOpts (tagged (fromRecord x) :: FnMintTokenInput)

type FnNameInput = Tagged "name()" Tuple0
type FnNameOutput = Tuple1 String

name :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError String)
name txOpts chainCursor = map unTuple1 <$> call txOpts chainCursor (tagged Tuple0 :: FnNameInput)

type FnOwnerOfInput = Tagged "ownerOf(uint256)" (Tuple1 (Tagged "tokenId" (Identity (UIntN 256))))
type FnOwnerOfOutput = Tuple1 Address

ownerOf
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { tokenId :: UIntN 256 }
  -> Web3 (Either CallError Address)
ownerOf txOpts chainCursor x = map unTuple1 <$> call txOpts chainCursor
  (tagged (fromRecord x) :: FnOwnerOfInput)

type FnSafeTransferFromb88d4fdeInput = Tagged
  "safeTransferFromb88d4fde(address,address,uint256,bytes)"
  ( Tuple4 (Tagged "from" (Identity Address)) (Tagged "to" (Identity Address))
      (Tagged "tokenId" (Identity (UIntN 256)))
      (Tagged "data" (Identity ByteString))
  )

type FnSafeTransferFromb88d4fdeOutput = Tuple0

safeTransferFromb88d4fde
  :: TransactionOptions NoPay
  -> { from :: Address, to :: Address, tokenId :: UIntN 256, data :: ByteString }
  -> Web3 HexString
safeTransferFromb88d4fde txOpts x = sendTx txOpts
  (tagged (fromRecord x) :: FnSafeTransferFromb88d4fdeInput)

type FnSafeTransferFrom42842e0eInput = Tagged "safeTransferFrom42842e0e(address,address,uint256)"
  ( Tuple3 (Tagged "from" (Identity Address)) (Tagged "to" (Identity Address))
      (Tagged "tokenId" (Identity (UIntN 256)))
  )

type FnSafeTransferFrom42842e0eOutput = Tuple0

safeTransferFrom42842e0e
  :: TransactionOptions NoPay
  -> { from :: Address, to :: Address, tokenId :: UIntN 256 }
  -> Web3 HexString
safeTransferFrom42842e0e txOpts x = sendTx txOpts
  (tagged (fromRecord x) :: FnSafeTransferFrom42842e0eInput)

type FnSetApprovalForAllInput = Tagged "setApprovalForAll(address,bool)"
  (Tuple2 (Tagged "operator" (Identity Address)) (Tagged "approved" (Identity Boolean)))

type FnSetApprovalForAllOutput = Tuple0

setApprovalForAll
  :: TransactionOptions NoPay -> { operator :: Address, approved :: Boolean } -> Web3 HexString
setApprovalForAll txOpts x = sendTx txOpts (tagged (fromRecord x) :: FnSetApprovalForAllInput)

type FnSupportsInterfaceInput = Tagged "supportsInterface(bytes4)"
  (Tuple1 (Tagged "interfaceId" (Identity (BytesN 4))))

type FnSupportsInterfaceOutput = Tuple1 Boolean

supportsInterface
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { interfaceId :: BytesN 4 }
  -> Web3 (Either CallError Boolean)
supportsInterface txOpts chainCursor x = map unTuple1 <$> call txOpts chainCursor
  (tagged (fromRecord x) :: FnSupportsInterfaceInput)

type FnSymbolInput = Tagged "symbol()" Tuple0
type FnSymbolOutput = Tuple1 String

symbol :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError String)
symbol txOpts chainCursor = map unTuple1 <$> call txOpts chainCursor
  (tagged Tuple0 :: FnSymbolInput)

type FnTokenURIInput = Tagged "tokenURI(uint256)"
  (Tuple1 (Tagged "tokenId" (Identity (UIntN 256))))

type FnTokenURIOutput = Tuple1 String

tokenURI
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { tokenId :: UIntN 256 }
  -> Web3 (Either CallError String)
tokenURI txOpts chainCursor x = map unTuple1 <$> call txOpts chainCursor
  (tagged (fromRecord x) :: FnTokenURIInput)

type FnTransferFromInput = Tagged "transferFrom(address,address,uint256)"
  ( Tuple3 (Tagged "from" (Identity Address)) (Tagged "to" (Identity Address))
      (Tagged "tokenId" (Identity (UIntN 256)))
  )

type FnTransferFromOutput = Tuple0

transferFrom
  :: TransactionOptions NoPay
  -> { from :: Address, to :: Address, tokenId :: UIntN 256 }
  -> Web3 HexString
transferFrom txOpts x = sendTx txOpts (tagged (fromRecord x) :: FnTransferFromInput)
