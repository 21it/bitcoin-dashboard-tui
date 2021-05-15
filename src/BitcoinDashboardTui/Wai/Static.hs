{-# LANGUAGE TemplateHaskell #-}

module BitcoinDashboardTui.Wai.Static
  ( assetsHTML,
    metaHTML,
    headerHTML,
    middleware,
  )
where

import Concur.Replica
import qualified Data.FileEmbed as FE
import qualified Data.Map as Map (fromList)
import BitcoinDashboardTui.Import.External
import qualified Network.Wai.Middleware.StaticEmbedded as S

headerHTML :: HTML
headerHTML = metaHTML <> assetsHTML

metaHTML :: HTML
metaHTML =
  [ VLeaf
      "meta"
      ( fl
          [ ("name", AText "viewport"),
            ("content", AText "width=device-width, initial-scale=1")
          ]
      )
      Nothing
  ]

assetsHTML :: HTML
assetsHTML =
  [ VLeaf
      "link"
      ( fl
          [ ("href", AText "bootstrap.css"),
            ("rel", AText "stylesheet")
          ]
      )
      Nothing,
    VLeaf
      "link"
      ( fl
          [ ("href", AText "app.css"),
            ("rel", AText "stylesheet")
          ]
      )
      Nothing
  ]

fl :: [(Text, Attr)] -> Attrs
fl = Map.fromList

middleware :: Middleware
middleware =
  S.static
    [ ("bootstrap.css", $(FE.embedFile "./static/css/bootstrap.css")),
      ("app.css", $(FE.embedFile "./static/css/app.css"))
    ]
