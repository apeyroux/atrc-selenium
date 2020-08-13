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
firefoxConfig = useBrowser firefox defaultConfig 

chromeConfig :: WDConfig
-- chromeConfig = useBrowser (chrome { chromeBinary = Just "/nix/store/rsssqjzxs9c25ir9mzq074k50l26fzgp-user-environment/bin/google-chrome-stable", chromeOptions = ["--no-startup-window"] }) defaultConfig
-- chromeConfig = useBrowser (chrome { chromeBinary = Just "/nix/store/rsssqjzxs9c25ir9mzq074k50l26fzgp-user-environment/bin/google-chrome-stable" }) defaultConfig
chromeConfig = useBrowser chrome defaultConfig

phantomjsConfig :: WDConfig
phantomjsConfig = useBrowser (Phantomjs { phantomjsBinary = (Just "/nix/store/vk33lmblw5wyncq5n93y0z4nzjhjahy1-phantomjs-1.9.8/bin/phantomjs") , phantomjsOptions = [] }) defaultConfig

checkInstance :: Text -> IO ()
checkInstance instanceName = do

  putStrLn $ "Start du test pour " <> (T.unpack instanceURL) <> " ..."

  runSession chromeConfig . finallyClose $ do

    openPage (T.unpack instanceURL)
  
    loginInput <- findElem (ByCSS "input[type='text']")
    passwordInput <- findElem (ByCSS "input[type='password']")
  
    sendKeys "Administrator" loginInput
    sendKeys "Administrator" passwordInput
  
    submit passwordInput

    t <- getTitle
    liftIO $ putStrLn $ "Ouverture de la page: " <> T.unpack t

    divServiceBlock <- try $ findElem (ByCSS "div[class='userServicesBlock']")
    case (divServiceBlock :: Either FailedCommand Element) of
      Left (FailedCommand t _) -> do
        liftIO $ putStrLn "ERROR I TAKE SCREENSHOT !!!!"
        saveScreenshot $ "/tmp/snap/error-" <> (T.unpack instanceName) <> ".jpg"
        liftIO $ putStrLn $ show t
      Right sb -> do
        t <- getText sb
        liftIO $ print t

  putStrLn "Fin du test ..."

  where
    instanceURL = "https://" <> instanceName <> ".krb.gendarmerie.fr"


main :: IO ()
main = do
  inventaire <- BSL.readFile "./inventaire.json"
  case decode inventaire :: Maybe [Text] of
    Just i -> mapM_ (\i -> do
                        r <- try (checkInstance i)
                        case (r :: Either FailedCommand ()) of
                          Left (FailedCommand t _) -> do
                            putStrLn $ "ERROR: " <> show t
                          Right _ -> do
                            putStrLn "Traitement OK !!!!"
                    ) i
    Nothing -> putStrLn "Je ne comprend pas l'inventaire"
