module Main where
import           Data.OpEnergy.API
import           Data.OpEnergy.API.V1.Block
import           Data.Swagger
import           Data.Aeson
import           Servant.Swagger
import qualified Data.ByteString.Lazy.Char8 as BS

main :: IO ()
main = do
  BS.putStrLn $ encode apiSwagger 
  return ()
