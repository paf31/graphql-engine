module Main where

import           Hasura.Prelude

import           Control.Exception.Lifted (finally)
import           Control.Lens             ((^.), (^..))
import           Data.Aeson               ((.=))
import           Data.Aeson.Lens          (_String, key, values)
import           Data.String              (fromString)
import           Hasura.Db                (defaultTxErrorHandler)
import           Hasura.RQL.Types.Error   (QErr)
import           Test.Hspec               (describe, it)

import qualified Data.Aeson               as Aeson
import qualified Database.PG.Query        as Q
import qualified System.Environment       as Env
import qualified Network.Wreq             as Wreq
import qualified Test.Hspec               as Hspec
    
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
    
    flip finally teardown do
      setup
      liftIO $ Hspec.hspec do
        describe "v1/graphql" do
          it "responds to basic queries" do
            res <- Wreq.asValue =<< Wreq.post (endpoint <> "/v1/graphql") 
              (Aeson.object 
                [ "query" .= id @Text 
                    "query Customers { \
                    \  customer {      \
                    \    id,           \
                    \    name          \
                    \  }               \
                    \}"
                ])
            (res ^. Wreq.responseStatus . Wreq.statusCode) 
              `Hspec.shouldBe` 200
            (res ^.. Wreq.responseBody . key "data" . key "customer" . values . key "name" . _String) 
              `Hspec.shouldBe` ["John Smith"]
    
  either print pure e