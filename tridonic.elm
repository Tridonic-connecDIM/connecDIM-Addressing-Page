module Tridonic (pageHeader) where

import Html exposing (Html, div, fromElement)
import Html.Attributes as Attr exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Text

links : List (String, String)
links =
  [ ("Main", "/index.html")
  , ("Addressing", "/addressing.html")
  , ("Proxy Settings", "/proxy.html")
  ]

linksExcluding : String -> List (String, String)
linksExcluding linkName =
  List.filter
    (\(name, linkLocation) -> name /= linkName)
    links

pageHeader : (Int, Int) -> String -> Element
pageHeader (windowWidth, windowHeight) excludeLinkName =
  let
    logoHeight = 48
    logoWidth = 173
    headerBarHeight = 10
    bannerHeight = logoHeight + headerBarHeight
    preliminaryBannerWidth = (toFloat >> (*) 0.6 >> round) windowWidth
    minBannerWidth = 980
    bannerWidth =
      if preliminaryBannerWidth < minBannerWidth
      then minBannerWidth
      else preliminaryBannerWidth
    hyperLinks =
      linksExcluding excludeLinkName
      |> List.map (\(name, linkLocation) -> link linkLocation (Text.fromString name |> Text.color (rgb 116 195 219) |> Text.height 20 |> leftAligned))
      |> List.intersperse (spacer 20 20)
      |> flow right
    bannerContainer = container bannerWidth bannerHeight
    collagedElements =
      collage bannerWidth bannerHeight
        [ rect (toFloat bannerWidth) (toFloat bannerHeight)
          |> filled (rgb 0 43 93)
        , image logoWidth logoHeight "/img/tridonic_logo.png"
          |> bannerContainer topLeft
          |> toForm
        , tiledImage bannerWidth headerBarHeight "/img/header_hor_bar.png"
          |> bannerContainer midBottom
          |> toForm
        , hyperLinks
          |> bannerContainer (bottomLeftAt (absolute (logoWidth + 10)) (absolute 10))
          |> toForm
        ]
  in
    container windowWidth bannerHeight midTop collagedElements
