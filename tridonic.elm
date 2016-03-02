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

pageHeader : (Int, Int) -> String -> Html
pageHeader (windowWidth, windowHeight) excludeLinkName =
  let
    logoHeight = 48
    logoWidth = 173
    headerBarHeight = 10
    bannerHeight =
      logoHeight + headerBarHeight
    hyperLinks =
      linksExcluding excludeLinkName
      |> List.map (\(name, linkLocation) -> link linkLocation (leftAligned <| Text.color (rgb 116 195 219) <| Text.fromString name))
      |> List.intersperse (spacer 10 10)
      |> flow right
    bannerWidth = round ((toFloat windowWidth) * 0.6)
    bannerContainer = container bannerWidth bannerHeight
    collagedElements = collage bannerWidth bannerHeight
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
    div []
      [ fromElement <| container windowWidth bannerHeight midTop collagedElements
      ]
