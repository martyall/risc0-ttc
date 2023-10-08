module Deploy.Main (main, mkEnv) where

import Prelude

import Chanterelle.Deploy (deploy)
import Control.Error.Util (exceptNoteA, exceptNoteM)
import Control.Monad.Except (runExceptT)
import Data.Either (either)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Deploy (deployScript)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Network.Ethereum.Core.HexString (toByteString)
import Network.Ethereum.Web3 (BytesN, mkHexString)
import Network.Ethereum.Web3 as BytesN
import Node.Process as Env
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = do
  { imageId, nodeURL, timeoutSeconds } <- mkEnv
  launchAff_ $ do
    deployResults <- deploy nodeURL timeoutSeconds $ deployScript { imageId }
    Console.log $ "Deployment Successful\n" <> show deployResults

mkEnv :: Effect { imageId :: BytesN 32, nodeURL :: String, timeoutSeconds :: Int }
mkEnv = do
  eEnv <- runExceptT do
    imageId <- do
      imageId <- assertEnvVar "IMAGE_ID"
      let mBytes = BytesN.fromByteString (Proxy @32) <<< toByteString =<< mkHexString imageId
      exceptNoteM mBytes ("Could not parse as bytes32: " <> imageId)
    nodeURL <- liftEffect do
      mNodeURL <- Env.lookupEnv "NODE_URL"
      pure $ fromMaybe "http://localhost:8545" mNodeURL
    timeoutSeconds <- liftEffect do
      mTimeout <- Env.lookupEnv "TIMEOUT"
      pure $ fromMaybe 60 (fromString =<< mTimeout)
    pure { imageId, nodeURL, timeoutSeconds }
  either throw pure eEnv
  where
  assertEnvVar var = exceptNoteA (Env.lookupEnv var) ("Couldn't find environment variable: " <> var)
