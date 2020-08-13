{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception.Lifted (SomeException, try, throwIO)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.Text (Text)
import           Data.Text as T
import           Test.WebDriver
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

  runSession (firefoxConfig { wdHTTPRetryCount = 2 }) . finallyClose $ do

    openPage (T.unpack instanceURL)
  
    loginInput <- findElem (ByCSS "input[type='text']")
    passwordInput <- findElem (ByCSS "input[type='password']")
    loginButton <- findElem (ByCSS "input[class='login_button']")
  
    sendKeys "lrpgn" loginInput
    sendKeys "nvw6xvkoyo" passwordInput
  
    -- submit loginButton 
    click loginButton 

    -- t <- getTitle
    -- liftIO $ putStrLn $ "Ouverture de la page: " <> T.unpack t

    divServiceBlock <- try $ findElem (ByCSS "div[class='menu']")
    case (divServiceBlock :: Either FailedCommand Element) of
      Left (FailedCommand t _) -> do
        liftIO $ putStrLn $ (T.unpack instanceName) <> " [KO] " <> show t 
        saveScreenshot $ "/tmp/snap/error-" <> (T.unpack instanceName) <> ".png"
        -- liftIO $ putStrLn $ show t
      Right _ -> do
        -- t <- getText sb
        -- liftIO $ print t
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
