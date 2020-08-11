{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Exception.Lifted (SomeException, try, throwIO)
import Data.Text as T
import Test.WebDriver
import Test.WebDriver.Exceptions

firefoxConfig :: WDConfig
firefoxConfig = defaultConfig

chromeConfig :: WDConfig
chromeConfig = useBrowser (chrome { chromeBinary = Just "/nix/store/rsssqjzxs9c25ir9mzq074k50l26fzgp-user-environment/bin/google-chrome-stable" }) defaultConfig

main :: IO ()
main = do
  runSession chromeConfig . finallyClose $ do
    liftIO $ putStrLn "Start du test ..."

    openPage "http://127.0.0.1:8080"
  
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
        saveScreenshot "/tmp/ok.jpg"
        liftIO $ putStrLn $ show t
      Right sb -> do
        t <- getText sb
        liftIO $ print t

    closeSession
