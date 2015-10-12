
import Graphics.Collage exposing (..)
import Graphics.Element exposing (image, fittedImage, croppedImage, Element, centered)
import Color exposing (red, black, white)
import Window
import Time exposing (..)
import Text exposing (color, fromString)
import Debug exposing ( log )
import Keyboard exposing (..)


import Html exposing (Html, Attribute, text, toElement, div, input, h1, h3)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)


main : Signal Element
main = 
  Signal.map3 renderScene Window.dimensions (every (50 * millisecond)) (timestamp Keyboard.enter)
  --(fpsWhen 5 (Signal.constant True))

renderScene: (Int, Int) -> Float -> (Float, a) -> Element
renderScene (width, height) time (enterPressed, _) =
  let 
    angle = (toFloat ((round (6 * inSeconds time)) % 360))
    sinceEnter = round ((time - enterPressed) / 100)
    shrinkingHeight = toFloat (64 - (3 * sinceEnter))
    spinningAwayAngle = toFloat (60 + (24 * sinceEnter))
  in
    --log ("delay: " ++ (toString (inSeconds delay))) <|
    collage width height
      [ 
        --toForm (h1 [] [Html.text "Scream!"])
        rect (toFloat width) (toFloat height) |> filled black |> move(0, 0)
      , rotate (degrees angle) (toForm (image (width * 2) (height * 2) "/images/void_compressed.jpg"))
      --, rotate (degrees ((delay - 200.0) * 30)) (toForm (centered (Text.height (((delay - 200.0) + 1) * 8) (color white (fromString ("Message will go here " ++ (toString (delay - 200.0) )))))))
      , rotate (degrees spinningAwayAngle) (toForm (centered (Text.height shrinkingHeight  (color white (fromString ("Message will go here " ++ (toString sinceEnter)))))))
      , rotate (degrees angle) (toForm (centered (color red (fromString ("Scream into the void : " ++ (toString angle))))))
      ]
