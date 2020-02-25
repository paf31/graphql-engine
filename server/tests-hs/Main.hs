module Main where

import           Hasura.Prelude

import           Control.Exception.Lifted (finally)
import           Control.Lens             ((^.))
import           Data.Aeson               ((.=))
import           Data.Text                (unpack)
import           Data.Text.Encoding       (decodeUtf8)
import           System.FilePath.Posix    ((</>))
import           Test.Hspec.Core.Spec     (Example(..), describe, it)
import           Test.Hspec.Golden        (Golden(..))

import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Text.IO             as Text
import qualified System.Environment       as Env
import qualified Network.Wreq             as Wreq
import qualified Test.Hspec               as Hspec
    
newtype GoldenIO = GoldenIO (IO (Golden Aeson.Value))
    
instance Example GoldenIO where
  type Arg GoldenIO = ()
  evaluateExample (GoldenIO gio) p f k = gio >>= \g ->
    evaluateExample g p f k
    
main :: IO ()
main = do
  endpoint <- Env.getEnv "HASURA_HGE_URL"
  
  let runSQL sql =
        Wreq.post (endpoint <> "/v1/query") 
          (Aeson.object 
            [ "type" .= id @Text "run_sql"
            , "args" .= Aeson.object 
                [ "sql" .= id @Text sql
                ]
            ])
            
      trackTable tableName =
        Wreq.post (endpoint <> "/v1/query") 
          (Aeson.object 
            [ "type" .= id @Text "track_table"
            , "args" .= Aeson.object 
                [ "name" .= id @Text tableName
                ]
            ])
            
      untrackTable tableName = 
        Wreq.post (endpoint <> "/v1/query") 
          (Aeson.object 
            [ "type" .= id @Text "untrack_table"
            , "args" .= Aeson.object 
                [ "table" .= Aeson.object 
                    [ "name" .= id @Text tableName
                    ]
                , "cascade" .= True
                ]
            ])
  
      setup, teardown :: IO ()
      setup = do
        _ <- runSQL =<< Text.readFile "tests-hs/sql/setup.sql"
        _ <- trackTable "customer"
        pure ()

      teardown = do
        _ <- untrackTable "customer"
        _ <- runSQL =<< Text.readFile "tests-hs/sql/teardown.sql"
        pure ()
        
      testFile :: String -> GoldenIO
      testFile testName = GoldenIO do
        query <- Text.readFile ("tests-hs" </> "golden" </> testName </> "query")
        res <- Wreq.asValue =<< Wreq.post (endpoint <> "/v1/graphql") 
          (Aeson.object [ "query" .= query ])
        (res ^. Wreq.responseStatus . Wreq.statusCode) 
          `Hspec.shouldBe` 200
        pure Golden 
          { output = res ^. Wreq.responseBody
          , encodePretty = unpack . decodeUtf8 . BL.toStrict . Pretty.encodePretty
          , testName = testName
          , writeToFile = \path -> BL.writeFile path . Pretty.encodePretty
          , readFromFile = fmap (either error id) . Aeson.eitherDecodeFileStrict
          , directory = "tests-hs" </> "golden"
          }
  
  setup
  (`finally` teardown) do
    Hspec.hspec do
      describe "v1/graphql" do
        it "responds to basic queries" do
          testFile "query_all_customers"