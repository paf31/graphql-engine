module Main where

import           Hasura.Prelude

import           Control.Exception.Lifted (finally)
import           Control.Lens             ((^.))
import           Data.Aeson               ((.=))
import           Data.String              (fromString)
import           Data.Text                (unpack)
import           Data.Text.Encoding       (decodeUtf8)
import           Hasura.Db                (defaultTxErrorHandler)
import           Hasura.RQL.Types.Error   (QErr)
import           System.FilePath.Posix    ((</>))
import           Test.Hspec.Core.Spec     (Example(..), describe, it)
import           Test.Hspec.Golden        (Golden(..), defaultGolden)

import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Text.IO             as Text
import qualified Database.PG.Query        as Q
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
  connString <- Env.getEnv "HASURA_GRAPHQL_DATABASE_URL"
  endpoint <- Env.getEnv "HASURA_HGE_URL"
  
  let connInfo = Q.ConnInfo 5 (Q.CDDatabaseURI (fromString connString))
  pool <- Q.initPGPool connInfo Q.defaultConnParams (print @Q.PGLogEvent)

  e <- runExceptT do
    let trackTable tableName =
          Wreq.post (endpoint <> "/v1/query") 
            (Aeson.object 
              [ "type" .= id @Text "track_table"
              , "args" .= Aeson.object 
                  [ "name"   .= id @Text tableName
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
    
        setup, teardown :: ExceptT QErr IO ()
        setup = do
          Q.runTx pool (Q.Serializable, Nothing) do
            Q.unitQE defaultTxErrorHandler [Q.sql|
              CREATE TABLE IF NOT EXISTS customer
                ( id serial
                , name text NOT NULL
                )
              |] () True
            Q.unitQE defaultTxErrorHandler [Q.sql|
              INSERT INTO customer (name)
              VALUES ($1)
              |] (Identity ("John Smith" :: Text)) True
          
          _ <- liftIO $ trackTable "customer"
          
          pure ()

        teardown = Q.runTx pool (Q.Serializable, Nothing) do
          _ <- liftIO $ untrackTable "customer"
          
          Q.unitQE defaultTxErrorHandler 
            [Q.sql| DROP TABLE customer |] 
            () True
    
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
    
    flip finally teardown do
      setup
      liftIO $ Hspec.hspec do
        describe "v1/graphql" do
          it "responds to basic queries" do
            testFile "query_all_customers"
    
  either print pure e