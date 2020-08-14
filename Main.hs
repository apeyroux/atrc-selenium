{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Control.Exception.Lifted (SomeException, try, throwIO)
import           Control.Monad (liftM2)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Lazy
import           Control.Monad.Trans.Writer.Lazy as WL
import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.Monoid
import           Data.Text (Text)
import           Data.Text as T
import           GHC.Generics
import           Network.HTTP.Conduit (ResponseTimeout)
import           Test.WebDriver
import           Test.WebDriver.Commands.Wait
import           Test.WebDriver.Exceptions

firefoxConfig :: WDConfig
firefoxConfig = useBrowser firefox { ffAcceptInsecureCerts = (Just True)  }  defaultConfig 

chromeConfig :: WDConfig
-- chromeConfig = useBrowser (chrome { chromeBinary = Just "/nix/store/rsssqjzxs9c25ir9mzq074k50l26fzgp-user-environment/bin/google-chrome-stable", chromeOptions = ["--no-startup-window"] }) defaultConfig
-- chromeConfig = useBrowser (chrome { chromeBinary = Just "/nix/store/q31sq1y9kwbcdx2d97iz48zpqrdhnl1a-chromium-83.0.4103.97/bin/chromium" }) defaultConfig
chromeConfig = useBrowser chrome defaultConfig

phantomjsConfig :: WDConfig
phantomjsConfig = useBrowser (Phantomjs { phantomjsBinary = (Just "/nix/store/vk33lmblw5wyncq5n93y0z4nzjhjahy1-phantomjs-1.9.8/bin/phantomjs") , phantomjsOptions = [] }) defaultConfig

data InstanceState = InstanceState {
  isName :: Text
  , isOk :: Bool
  , isMessage :: Maybe String
  , isScreenPath :: Maybe String
} deriving (Eq, Ord, Show, Generic)

instance FromJSON InstanceState
instance ToJSON InstanceState

-- checkInstance :: Text -> InstanceState IO ()
checkInstance instanceName = do

  -- putStrLn $ "Start du test pour " <> (T.unpack instanceURL) <> " ..."

  liftIO $ runSession (firefoxConfig { wdHTTPRetryCount = 1 }) . finallyClose $ do

    opage <- try $ openPage (T.unpack instanceURL)
    case (opage :: Either SomeException ()) of
      Left x -> do
        liftIO $ putStrLn $ (T.unpack instanceName) <> " [KO] (prob de cnx)"
        return InstanceState {
                  isName = instanceName
                  , isOk = False
                  , isMessage = Just $ show x
                  , isScreenPath = Nothing
                  }
      Right _ -> do

        -- t <- getTitle
        -- liftIO $ putStrLn $ "Ouverture de la page: " <> T.unpack t <> " sur " <> (T.unpack instanceName)
  
        loginInput <- findElem (ById "username")
        passwordInput <- findElem (ById "password")
        loginButton <- findElem (ByClass "login_button")
  
        sendKeys "lrpgn" loginInput
        sendKeys "nvw6xvkoyo" passwordInput
  
        -- waitUntil 30 $ submit passwordInput
        -- waitUntil 30 $ click loginButton
        click loginButton

        -- t <- getTitle
        -- liftIO $ putStrLn $ "Ouverture de la page: " <> T.unpack t <> " sur " <> (T.unpack instanceName)

        -- searchForm <- try $ waitUntil 120 $ findElem (ById "nxw_search_form:nxw_search")
        searchForm <- try $ waitUntil 30 $ findElem (ById "nxw_search_form:nxw_search")
        case (searchForm :: Either FailedCommand Element) of
          Left (FailedCommand t x) -> do
            saveScreenshot $ "/tmp/snap/" <> (T.unpack instanceName) <> ".png"
            -- liftIO $ putStrLn $ (T.unpack instanceName) <> " [KO] " <> show t <> " " <> show x
            liftIO $ putStrLn $ (T.unpack instanceName) <> " [KO] (voir screenshot) " <> show t
            return InstanceState {
              isName = instanceName
              , isOk = False
              , isMessage = Just $ show x
              , isScreenPath = Just $ "/tmp/snap/" <> (T.unpack instanceName) <> ".png"
              }
            -- liftIO $ putStrLn $ show t
          Right sf -> do
            -- t <- getText sb
            -- liftIO $ print t
            -- searchButtonText <- getText sb
            -- liftIO $ putStrLn (T.unpack searchButtonText)
            click sf

            -- t <- getTitle
            -- liftIO $ putStrLn $ "Ouverture de la page: " <> T.unpack t <> " sur " <> (T.unpack instanceName)
            searchButton <- try $ waitUntil 30 $ findElem (ById "nxl_gridSearchLayout:nxw_searchLayout_form:nxw_searchActions_submitSearch")
    
            case (searchButton :: Either FailedCommand Element) of
              Left (FailedCommand t x) -> do
                saveScreenshot $ "/tmp/snap/" <> (T.unpack instanceName) <> ".png"
                -- liftIO $ putStrLn $ (T.unpack instanceName) <> " [KO] " <> show t <> " " <> show x
                liftIO $ putStrLn $ (T.unpack instanceName) <> " [KO] (voir screenshot)" <> show t
                return InstanceState {
                  isName = instanceName
                  , isOk = False
                  , isMessage = Just $ show x
                  , isScreenPath = Just $ "/tmp/snap/" <> (T.unpack instanceName) <> ".png"
                  }
                -- liftIO $ putStrLn $ show t
              Right sb -> do
                -- t <- getText sb
                -- liftIO $ print t
                -- searchButtonText <- getText sb
                -- liftIO $ putStrLn (T.unpack searchButtonText)
                saveScreenshot $ "/tmp/snap/" <> (T.unpack instanceName) <> ".png"
                liftIO $ putStrLn $ (T.unpack instanceName) <> " [OK]"
                return InstanceState {
                  isName = instanceName
                  , isOk = True
                  , isMessage = Nothing
                  , isScreenPath = Just $ "/tmp/snap/" <> (T.unpack instanceName) <> ".png"
                  }

  -- putStrLn "Fin du test ..."

  where
    instanceURL = "https://atrc-" <> instanceName <> ".krb.gendarmerie.fr/nuxeo/login.jsp"

main :: IO ()
main = do
  (_, w) <- runWriterT $ do
    inventaire <- liftIO $ BSL.readFile "./inventaire.json"
    case decode inventaire :: Maybe [Text] of
      Just i -> mapM_ (\i -> do
                        r <- try $ checkInstance i
                        case (r :: Either FailedCommand InstanceState) of
                          Left (FailedCommand s t) -> do
                            -- tell [s]
                            -- putStrLn $ (T.unpack i) <> " [KO]" -- "ERROR: " <> show t
                            tell [InstanceState {
                              isName = i
                              , isOk = False
                              , isMessage = Just $ show t
                              , isScreenPath = Nothing
                              }]
                            return ()
                          Right s -> do
                            tell [s]
                            return ()
                            -- putStrLn "Traitement OK !!!!"
                    ) i
      Nothing -> liftIO $ putStrLn "Je ne comprend pas l'inventaire"
  BSL.putStrLn $ encode w
  BSL.writeFile "./status.json" (encode w)

