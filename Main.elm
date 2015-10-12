
import Graphics.Collage exposing (..)
import Graphics.Element exposing (image, fittedImage, croppedImage, Element, centered, show, middle, container)
import Color exposing (red, black, white)
import Window
import Time exposing (..)
import Text exposing (color, fromString)
import Debug exposing ( log )
import Keyboard exposing (..)
import Html exposing (Html, fromElement, Attribute, text, toElement, div, input, h1, h2, node, button)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)

--import StartApp.Simple exposing (start)

type alias Model = String

main : Signal Html
main = 
  Signal.map3 renderScene Window.dimensions (every (50 * millisecond)) (timestamp Keyboard.enter)
  --(fpsWhen 5 (Signal.constant True))


heading: Html
heading =
  div [style [("position", "absolute"),
          ("z-index", "100"),
          ("width", "100%")]] 
  [
    h1 [style [("text-align", "center"), ("margin", "0"), ("color", "white"), ("font-size", "96pt"), ("font-family", "Creepster")]] [Html.text "Scream"]
  , h2 [style [("text-align", "center"), ("margin", "0"), ("color", "white"), ("font-size", "48pt"), ("font-family", "Creepster")]] [Html.text "into the void"]
  ]

inputArea : Html
inputArea =
  --fromElement (container 300 300 middle (show "Try this with html."))
  div [style [("position", "absolute"),
          ("z-index", "100"),
          ("width", "100%"),
          ("height", "100%"),
          ("display", "flex"),
          ("justify-content", "center"),
          ("align-items", "center")
          ]] 
  [
    input [style []] []
  , button [style []] [Html.text "Scream!"]
  ]

css : String -> Html
css path =
  node "link" [ rel "stylesheet", href path ] []

renderScene: (Int, Int) -> Float -> (Float, a) -> Html
renderScene (width, height) time (enterPressed, _) =
  let 
    angle = (toFloat ((round (6 * inSeconds time)) % 360))
    sinceEnter = round ((time - enterPressed) / 100)
    shrinkingHeight = toFloat (72 - (3 * sinceEnter))
    spinningAwayAngle = toFloat (60 + (24 * sinceEnter))
  in
    --log ("delay: " ++ (toString (inSeconds delay))) <|
    div [] 
    [
      css "https://fonts.googleapis.com/css?family=Creepster"
    ,  heading
    ,  inputArea
    ,  fromElement (collage width height
      [ 
        rect (toFloat width) (toFloat height) |> filled black |> move(0, 0)
      , rotate (degrees angle) (toForm (image (width * 2) (height * 2) "/images/void_compressed.jpg"))
      , rotate (degrees spinningAwayAngle) (toForm (centered (Text.height shrinkingHeight  (color white (fromString ("Message will go here " ++ (toString sinceEnter)))))))
      , rotate (degrees angle) (toForm (centered (color red (fromString ("Scream into the void : " ++ (toString angle))))))
      ])
    ]
