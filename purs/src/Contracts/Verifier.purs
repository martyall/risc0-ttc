module Contracts.Verifier where

import Prelude

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Identity (Identity)
import Network.Ethereum.Web3 (Vector, Web3, call)
import Network.Ethereum.Web3.Solidity (ByteString, BytesN, Tuple1, Tuple2, Tuple4, Tuple5, UIntN, fromRecord, unTuple1)
import Network.Ethereum.Web3.Types (CallError, ChainCursor, NoPay, TransactionOptions)

type FnVerify6efef009Input = Tagged "verify6efef009(bytes,bytes32,bytes32,bytes32)"
  ( Tuple4 (Tagged "seal" (Identity ByteString)) (Tagged "imageId" (Identity (BytesN 32)))
      (Tagged "postStateDigest" (Identity (BytesN 32)))
      (Tagged "journalHash" (Identity (BytesN 32)))
  )

type FnVerify6efef009Output = Tuple1 Boolean

verify6efef009
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { seal :: ByteString
     , imageId :: BytesN 32
     , postStateDigest :: BytesN 32
     , journalHash :: BytesN 32
     }
  -> Web3 (Either CallError Boolean)
verify6efef009 txOpts chainCursor x = map unTuple1 <$> call txOpts chainCursor
  (tagged (fromRecord x) :: FnVerify6efef009Input)

type FnVerify1a36ed75Input = Tagged "verify1a36ed75(bytes,bytes32,bytes32,bytes)"
  ( Tuple4 (Tagged "seal" (Identity ByteString)) (Tagged "imageId" (Identity (BytesN 32)))
      (Tagged "postStateDigest" (Identity (BytesN 32)))
      (Tagged "journal" (Identity ByteString))
  )

type FnVerify1a36ed75Output = Tuple1 Boolean

verify1a36ed75
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { seal :: ByteString
     , imageId :: BytesN 32
     , postStateDigest :: BytesN 32
     , journal :: ByteString
     }
  -> Web3 (Either CallError Boolean)
verify1a36ed75 txOpts chainCursor x = map unTuple1 <$> call txOpts chainCursor
  (tagged (fromRecord x) :: FnVerify1a36ed75Input)

type FnVerify04b18430Input = Tagged
  "verify04b18430((bytes,(bytes32,bytes32,(uint8,uint8),bytes32,bytes32)))"
  ( Tuple1
      ( Tagged "receipt"
          ( Tuple2 (Tagged "seal" (Identity ByteString))
              ( Tagged "meta"
                  ( Tuple5 (Tagged "preStateDigest" (Identity (BytesN 32)))
                      (Tagged "postStateDigest" (Identity (BytesN 32)))
                      ( Tagged "exitCode"
                          ( Tuple2 (Tagged "system" (Identity (UIntN 8)))
                              (Tagged "user" (Identity (UIntN 8)))
                          )
                      )
                      (Tagged "input" (Identity (BytesN 32)))
                      (Tagged "output" (Identity (BytesN 32)))
                  )
              )
          )
      )
  )

type FnVerify04b18430Output = Tuple1 Boolean

verify04b18430
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { receipt ::
         { seal :: ByteString
         , meta ::
             { preStateDigest :: BytesN 32
             , postStateDigest :: BytesN 32
             , exitCode :: { system :: UIntN 8, user :: UIntN 8 }
             , input :: BytesN 32
             , output :: BytesN 32
             }
         }
     }
  -> Web3 (Either CallError Boolean)
verify04b18430 txOpts chainCursor x = map unTuple1 <$> call txOpts chainCursor
  (tagged (fromRecord x) :: FnVerify04b18430Input)

type FnVerifyProofInput = Tagged "verifyProof(uint256[2],uint256[2][2],uint256[2],uint256[4])"
  ( Tuple4 (Tagged "_pA" (Identity (Vector 2 (UIntN 256))))
      (Tagged "_pB" (Identity (Vector 2 (Vector 2 (UIntN 256)))))
      (Tagged "_pC" (Identity (Vector 2 (UIntN 256))))
      (Tagged "_pubSignals" (Identity (Vector 4 (UIntN 256))))
  )

type FnVerifyProofOutput = Tuple1 Boolean

verifyProof
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { _pA :: Vector 2 (UIntN 256)
     , _pB :: Vector 2 (Vector 2 (UIntN 256))
     , _pC :: Vector 2 (UIntN 256)
     , _pubSignals :: Vector 4 (UIntN 256)
     }
  -> Web3 (Either CallError Boolean)
verifyProof txOpts chainCursor x = map unTuple1 <$> call txOpts chainCursor
  (tagged (fromRecord x) :: FnVerifyProofInput)
