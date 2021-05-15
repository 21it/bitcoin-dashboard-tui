module BitcoinDashboardTui.Class
  ( EnvM (..),
  )
where

import BitcoinDashboardTui.Data.Env (Env (..))
import BitcoinDashboardTui.Import.External

class MonadIO m => EnvM m where
  getEnv :: m Env
