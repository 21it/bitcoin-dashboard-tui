{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main,
  )
where

import BitcoinDashboardTui.Data.AppM (runApp)
import BitcoinDashboardTui.Import
import qualified BitcoinDashboardTui.Wai.Chat as Chat (app)
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  !rc <- rawConfig
  handleScribe <-
    mkHandleScribeWithFormatter
      ( case rawConfigLogFormat rc of
          Bracket -> bracketFormat
          JSON -> jsonFormat
      )
      ColorIfTerminal
      stdout
      (permitItem InfoS)
      (rawConfigLogVerbosity rc)
  let mkLogEnv =
        registerScribe "stdout" handleScribe defaultScribeSettings
          =<< initLogEnv "BitcoinDashboardTui" (Environment $ rawConfigLogEnv rc)
  bracket mkLogEnv closeScribes $ \le ->
    runKatipContextT le (mempty :: LogContexts) mempty $ do
      !env <- newEnv rc
      lift $ runApp env $ do
        res <- liftIO $ Warp.run (envEndpointPort env) $ Chat.app env
        $(logTM) ErrorS
          $ logStr
          $ "Terminate program with result " <> (show res :: Text)
