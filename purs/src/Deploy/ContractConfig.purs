module Deploy.ContractConfig
  ( tokenCfg
  , tokenLibCfg
  , ttcCfg
  , ttcLibCfg
  , testRelayCfg
  ) where

import Prelude

import Chanterelle.Types.Deploy (ContractConfig, LibraryConfig, NoArgs, constructorNoArgs, noArgs, validateWithError)
import Contracts.TTCTrading as TTCTrading
import Contracts.TestRelay as TestRelay
import Data.Int (fromString)
import Network.Ethereum.Web3 (Address, BytesN, UIntN, fromInt, uIntNFromBigNumber)
import Type.Proxy (Proxy(..))

testRelayCfg
  :: { chainId :: String }
  -> ContractConfig (chainId :: UIntN 256)
testRelayCfg { chainId } =
  { filepath: "build/contracts/TestRelay.json"
  , name: "TestRelay"
  , constructor: TestRelay.constructor
  , unvalidatedArgs: ado
      cId <- validateWithError mChainId ("Invalid chainId: " <> show chainId)
      in { chainId: cId }
  }
  where
  mChainId = fromString chainId >>= uIntNFromBigNumber (Proxy @256) <<< fromInt

tokenLibCfg :: LibraryConfig ()
tokenLibCfg =
  { filepath: "build/contracts/Token.json"
  , name: "Token"
  }

tokenCfg :: ContractConfig NoArgs
tokenCfg =
  { filepath: tokenLibCfg.filepath
  , name: tokenLibCfg.name
  , constructor: constructorNoArgs
  , unvalidatedArgs: noArgs
  }

ttcLibCfg :: LibraryConfig ()
ttcLibCfg =
  { filepath: "build/contracts/TTCTrading.json"
  , name: "TTCTrading"
  }

ttcCfg
  :: { token :: Address
     , relay :: Address
     , imageId :: BytesN 32
     }
  -> ContractConfig (_token :: Address, bonsaiRelay :: Address, _imageId :: BytesN 32)
ttcCfg args =
  { filepath: ttcLibCfg.filepath
  , name: ttcLibCfg.name
  , constructor: TTCTrading.constructor
  , unvalidatedArgs: pure
      { _token: args.token
      , bonsaiRelay: args.relay
      , _imageId: args.imageId
      }
  }
