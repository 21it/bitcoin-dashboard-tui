module Main (main) where

import BitcoinDashboardTui.Import
import Brick
import qualified Brick.AttrMap as AttrMap
import qualified Brick.BChan as Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.Table
import Coinbase.Exchange.MarketData
import Coinbase.Exchange.Types
import Coinbase.Exchange.Types.Core
import qualified Coinbase.Exchange.Types.MarketData as MarketData
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Graphics.Vty as Vty
import qualified Haskii.Figlet as FLF
import qualified Haskii.Figlet.Font as FLF
import qualified Haskii.Text as FLF
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import Text.Pretty.Simple
import Text.Printf (printf)
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
  deriving (Eq, Ord, Show, Num)

data MarketData
  = MarketDataStats MarketData.Stats ErrorCount
  | MarketDataError ExchangeFailure
  deriving (Show)

data MarketEvent
  = MarketEvent
      CurrencyPair
      (Either ExchangeFailure MarketData.Stats)
  deriving (Show)

newtype MarketState
  = MarketState (Map CurrencyPair MarketData)
  deriving (Show)

data ViewPort = ViewPort
  deriving (Eq, Ord, Show)

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
                Left . ApiFailure . TL.toStrict . pShowNoColor $
                  Http.HttpExceptionRequest
                    re
                      { Http.requestHeaders =
                          second (const "SECRET")
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

spawnLinkMarketWatcher :: Brick.BChan MarketEvent -> IO (Async ())
spawnLinkMarketWatcher chan = do
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
      Brick.writeBChan chan e
      delay 1000000

currencyPairs :: [CurrencyPair]
currencyPairs =
  [ CurrencyPair "BTC" "USD",
    CurrencyPair "BTC" "EUR",
    --CurrencyPair "XMR" "BTC",
    CurrencyPair "ADA" "BTC"
  ]

ui :: (Text -> Text) -> MarketState -> [Widget ViewPort]
ui toAscii =
  (: [])
    . center
    . vBox
    . tables toAscii

tables :: (Text -> Text) -> MarketState -> [Widget ViewPort]
tables toAscii (MarketState kv) =
  case partitionEithers . catMaybes $
    (\k -> row k <$> Map.lookup k kv) <$> currencyPairs of
    ([], xs) -> [tableOk xs]
    (es, []) -> [tableError es]
    (es, xs) -> [tableOk xs, tableError es]
  where
    showNum :: Double -> Text
    showNum = T.pack . printf "%.2f"
    tableOk =
      center
        . renderTable
        . table
        . (headerOk :)
    tableError =
      center
        . viewport ViewPort Both
        . renderTable
        . table
        . (headerError :)
    headerOk =
      txt . toAscii
        <$> [ "BASE/QUOTE",
              --"VOLUME",
              "OPEN",
              "LOW",
              "HIGH"
            ]
    headerError =
      [ txt "BASE/QUOTE",
        txt "ERROR"
      ]
    row k = \case
      MarketDataError err ->
        let e = case err of
              ParseFailure x ->
                "ParseFailure\n\n" <> x
              ApiFailure x ->
                "ApiFailure\n\n" <> x
              AuthenticationRequiredFailure x ->
                "AuthenticationRequiredFailure\n\n" <> x
              AuthenticationRequiresByteStrings ->
                "AuthenticationRequiresByteStrings"
         in Left
              [ txt $ show k,
                txt e
              ]
      MarketDataStats s _ ->
        Right $
          txt . toAscii
            <$> [ show k,
                  --showNum . unVolume $ statsVolume s,
                  showNum . unOpen $ statsOpen s,
                  showNum . unLow $ statsLow s,
                  showNum . unHigh $ statsHigh s
                ]

app :: (Text -> Text) -> Brick.App MarketState MarketEvent ViewPort
app toAscii =
  Brick.App
    { Brick.appDraw = ui toAscii,
      Brick.appChooseCursor = Brick.neverShowCursor,
      Brick.appHandleEvent = appEventHandler,
      Brick.appStartEvent = pure,
      Brick.appAttrMap = const $ AttrMap.attrMap Vty.defAttr []
    }

scroll :: Brick.ViewportScroll ViewPort
scroll = Brick.viewportScroll ViewPort

appEventHandler ::
  MarketState ->
  BrickEvent ViewPort MarketEvent ->
  EventM ViewPort (Next MarketState)
appEventHandler st@(MarketState s) =
  \case
    AppEvent (MarketEvent k v) ->
      continue . MarketState $
        Map.insertWith updateState k (createState v) s
    VtyEvent e ->
      case e of
        Vty.EvKey Vty.KDown [] -> scrollDown
        Vty.EvKey Vty.KUp [] -> scrollUp
        Vty.EvKey Vty.KRight [] -> scrollRight
        Vty.EvKey Vty.KLeft [] -> scrollLeft
        Vty.EvKey Vty.KEsc [] -> terminate
        Vty.EvKey (Vty.KChar 'q') [] -> terminate
        Vty.EvKey (Vty.KChar x) [Vty.MCtrl] | x `elem` ['c', 'd'] -> terminate
        _ -> continue st
    MouseDown {} ->
      continue st
    MouseUp {} ->
      continue st
  where
    scrollDown = Brick.vScrollBy scroll 1 >> Brick.continue st
    scrollUp = Brick.vScrollBy scroll (-1) >> Brick.continue st
    scrollRight = Brick.hScrollBy scroll 1 >> Brick.continue st
    scrollLeft = Brick.hScrollBy scroll (-1) >> Brick.continue st
    terminate = Brick.halt st
    createState = \case
      Left e -> MarketDataError e
      Right x -> MarketDataStats x 0
    updateState newMarket oldMarket =
      case (oldMarket, newMarket) of
        (MarketDataStats {}, MarketDataStats {}) -> newMarket
        (MarketDataStats _ ec, _) | ec > 10 -> newMarket
        (MarketDataStats x ec, MarketDataError {}) -> MarketDataStats x (ec + 1)
        (MarketDataError {}, _) -> newMarket

main :: IO ()
main = do
  let flf = FLF.getFLF FLF.Small
  let toAscii = TL.toStrict . FLF.render . FLF.figString flf . T.unpack
  chan <- Brick.newBChan 10
  void $ spawnLinkMarketWatcher chan
  initialVty <- buildVty
  void
    $ customMain
      initialVty
      buildVty
      (Just chan)
      (app toAscii)
    $ MarketState mempty
  where
    buildVty = Vty.mkVty Vty.defaultConfig
