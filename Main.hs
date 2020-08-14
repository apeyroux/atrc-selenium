{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception.Lifted (SomeException, try, throwIO)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.Text (Text)
import           Data.Text as T
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

checkInstance :: Text -> IO ()
checkInstance instanceName = do

  -- putStrLn $ "Start du test pour " <> (T.unpack instanceURL) <> " ..."

  runSession (firefoxConfig { wdHTTPRetryCount = 5 }) . finallyClose $ do

    opage <- try $ openPage (T.unpack instanceURL)
    case (opage :: Either SomeException ()) of
      Left x -> do
        liftIO $ putStrLn $ (T.unpack instanceName) <> " [KO] " <> " " <> show x
        return ()
      Right r ->
        return ()

    t <- getTitle
    liftIO $ putStrLn $ "Ouverture de la page: " <> T.unpack t <> " sur " <> (T.unpack instanceName)
  
    loginInput <- findElem (ById "username")
    passwordInput <- findElem (ById "password")
    loginButton <- findElem (ByClass "login_button")
  
    sendKeys "lrpgn" loginInput
    sendKeys "nvw6xvkoyo" passwordInput
  
    -- waitUntil 30 $ submit passwordInput
    -- waitUntil 30 $ click loginButton
    click loginButton

    t <- getTitle
    liftIO $ putStrLn $ "Ouverture de la page: " <> T.unpack t <> " sur " <> (T.unpack instanceName)

    -- searchForm <- try $ waitUntil 120 $ findElem (ById "nxw_search_form:nxw_search")
    searchForm <- findElem (ById "nxw_search_form:nxw_search")
    click searchForm

    t <- getTitle
    liftIO $ putStrLn $ "Ouverture de la page: " <> T.unpack t <> " sur " <> (T.unpack instanceName)
    searchButton <- try $ waitUntil 30 $ findElem (ById "nxl_gridSearchLayout:nxw_searchLayout_form:nxw_searchActions_submitSearch")
    
    case (searchButton :: Either FailedCommand Element) of
      Left (FailedCommand t x) -> do
        saveScreenshot $ "/tmp/snap/error-" <> (T.unpack instanceName) <> "-" <> show t <> ".png"
        liftIO $ putStrLn $ (T.unpack instanceName) <> " [KO] " <> show t <> " " <> show x
        -- liftIO $ putStrLn $ show t
      Right sb -> do
        -- t <- getText sb
        -- liftIO $ print t
        -- searchButtonText <- getText sb
        -- liftIO $ putStrLn (T.unpack searchButtonText)
        liftIO $ putStrLn $ (T.unpack instanceName) <> " [OK]"

  -- putStrLn "Fin du test ..."

  where
    instanceURL = "https://atrc-" <> instanceName <> ".krb.gendarmerie.fr/nuxeo/login.jsp"


main :: IO ()
main = do
  inventaire <- BSL.readFile "./inventaire.json"
  case decode inventaire :: Maybe [Text] of
    Just i -> mapM_ (\i -> do
                        r <- try (checkInstance i)
                        case (r :: Either FailedCommand ()) of
                          Left (FailedCommand t _) -> do
                            -- putStrLn $ (T.unpack i) <> " [KO]" -- "ERROR: " <> show t
                            return ()
                          Right _ -> do
                            return ()
                            -- putStrLn "Traitement OK !!!!"
                    ) i
    Nothing -> putStrLn "Je ne comprend pas l'inventaire"
