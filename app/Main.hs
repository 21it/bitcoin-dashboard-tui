module Main (main) where

import BitcoinDashboardTui.Import
import Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.Table
import Coinbase.Exchange.MarketData
import Coinbase.Exchange.Types
import Coinbase.Exchange.Types.Core
import qualified Coinbase.Exchange.Types.MarketData as MarketData
import qualified Data.Text as T
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import qualified Text.Show

class ToCoinbase a b where
  toCoinbase :: a -> b

data CurrencyRelation
  = Base
  | Quote

newtype CurrencyCode (a :: CurrencyRelation)
  = CurrencyCode Text
  deriving newtype (Eq, Ord, IsString)

data CurrencyPair
  = CurrencyPair (CurrencyCode 'Base) (CurrencyCode 'Quote)
  deriving (Eq, Ord)

newtype ErrorCount
  = ErrorCount Integer
  deriving (Eq, Ord, Show)

data MarketData
  = MarketDataStats MarketData.Stats ErrorCount
  | MarketDataError Text
  deriving (Show)

data MarketEvent
  = MarketEvent
      CurrencyPair
      (Either ExchangeFailure MarketData.Stats)
  deriving (Show)

newtype AppState
  = AppState (Map CurrencyPair MarketData)
  deriving (Show)

instance Text.Show.Show CurrencyPair where
  show (CurrencyPair base quote) =
    this base <> "/" <> this quote
    where
      this :: CurrencyCode a -> String
      this = T.unpack . coerce

instance ToCoinbase CurrencyPair ProductId where
  toCoinbase (CurrencyPair base quote) =
    ProductId $ coerce base <> "-" <> coerce quote

getMarketEvent :: ExchangeConf -> CurrencyPair -> IO MarketEvent
getMarketEvent conf cp =
  catches expr handlers
  where
    expr = MarketEvent cp <$> (runExchange conf . getStats $ toCoinbase cp)
    handlers =
      [ Handler
          ( \(e :: Http.HttpException) -> pure $ MarketEvent cp $ case e of
              (Http.InvalidUrlException _ _) -> Left $ ApiFailure $ show e
              (Http.HttpExceptionRequest re ex) ->
                Left $ ApiFailure $ show $
                  Http.HttpExceptionRequest
                    re
                      { Http.requestHeaders =
                          (second $ const "SECRET")
                            <$> Http.requestHeaders re,
                        Http.queryString = "SECRET"
                      }
                    ex
          )
      ]

newExchangeConf :: IO ExchangeConf
newExchangeConf = do
  mgr <- Http.newManager Http.tlsManagerSettings
  pure $
    ExchangeConf
      { manager = mgr,
        authToken = Nothing,
        apiType = Live
      }

spawnLinkMarketWatcher :: IO (Async ())
spawnLinkMarketWatcher = do
  conf <- newExchangeConf
  pid <- async $ loop conf
  link pid
  pure pid
  where
    loop conf = do
      mapM_ (this conf) currencyPairs
      loop conf
    this conf cp = do
      e <- getMarketEvent conf cp
      print e
      delay 1000000

currencyPairs :: [CurrencyPair]
currencyPairs =
  [ CurrencyPair "BTC" "USD",
    CurrencyPair "BTC" "EUR",
    CurrencyPair "ADA" "BTC",
    CurrencyPair "XMR" "BTC"
  ]

ui :: Widget ()
ui =
  center $
    renderTable leftTable

leftTable :: Table ()
leftTable =
  table
    [ [txt "Left", txt "Center", txt "Right"],
      [txt "X", txt "Some things", txt "A"],
      [txt "Z", txt "centered", txt "C"]
    ]

main :: IO ()
main = do
  void $ spawnLinkMarketWatcher
  delay 1000000000000
  simpleMain ui
