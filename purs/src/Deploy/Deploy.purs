module Deploy
  ( DeployResults
  , deployScript
  ) where

import Prelude

import Chanterelle.Deploy (deployContract)
import Chanterelle.Test (assertWeb3)
import Chanterelle.Types.Deploy (DeployConfig(..), DeployM)
import Control.Monad.Reader (ask)
import Data.Lens ((?~))
import Deploy.ContractConfig (testRelayCfg, tokenCfg, ttcCfg)
import Effect.Aff.Class (liftAff)
import Network.Ethereum.Web3 (Address, BytesN, _from, defaultTransactionOptions)
import Network.Ethereum.Web3.Api (net_version)

type DeployResults =
  { token :: Address
  , tokenOwner :: Address
  , testRelay :: Address
  , ttc :: Address
  }

deployScript :: { imageId :: BytesN 32 } -> DeployM DeployResults
deployScript { imageId } = do
  (DeployConfig { primaryAccount, provider }) <- ask
  let txOpts = defaultTransactionOptions # _from ?~ primaryAccount
  chainId <- liftAff $ assertWeb3 provider net_version
  { deployAddress: testRelay } <- deployContract txOpts $ testRelayCfg { chainId }
  let tokenOwner = primaryAccount
  { deployAddress: token } <- deployContract (txOpts # _from ?~ tokenOwner) tokenCfg
  { deployAddress: ttc } <- deployContract txOpts $ ttcCfg { token, relay: testRelay, imageId }
  pure
    { token
    , tokenOwner
    , testRelay
    , ttc
    }
