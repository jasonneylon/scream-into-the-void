
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
import Signal exposing (Address, mailbox)

--import StartApp.Simple exposing (start)

type alias Model = String

main : Signal Html
main = 
  Signal.map4 renderScene myMailbox.signal (every (50 * millisecond)) (timestamp Keyboard.enter) Window.dimensions
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

inputArea : String -> Bool -> Html
inputArea message show =
  let 
    inputStyles = if show then [("display", "none")] else [("display", "block"), ("width", "400px"), ("height", "100px"), ("font-size", "32pt")]
  in
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
      input [ style inputStyles
            , placeholder "What's up?"
            , value message
            , on "input" targetValue (Signal.message myMailbox.address)] 
          []
    --, button [style []] [Html.text "Scream!"]
    ]

css : String -> Html
css path =
  node "link" [ rel "stylesheet", href path ] []

renderScene: String -> Float -> (Float, a) -> (Int, Int) -> Html
renderScene message time (enterPressed, _) (width, height) =
  let 
    angle = (toFloat ((round (6 * inSeconds time)) % 360))
    sinceEnter = round ((time - enterPressed) / 100)
    shrinkingHeight = toFloat (72 - (3 * sinceEnter))
    spinningAwayAngle = toFloat (60 + (24 * sinceEnter))
    showInput = (sinceEnter < 40)
    spinnyMessage = if showInput then message else ""
  in
    --log ("delay: " ++ (toString (inSeconds delay))) <|
    div [] 
    [
      css "https://fonts.googleapis.com/css?family=Creepster"
    ,  heading
    ,  inputArea message showInput
    ,  fromElement (collage width height
      [ 
        rect (toFloat width) (toFloat height) |> filled black |> move(0, 0)
      , rotate (degrees angle) (toForm (image (width * 2) (height * 2) "/images/void_compressed.jpg"))
      , rotate (degrees spinningAwayAngle) (toForm (centered (Text.height shrinkingHeight (color white (fromString spinnyMessage)))))
      --, rotate (degrees angle) (toForm (centered (color red (fromString ("Scream into the void : " ++ (toString angle))))))
      ])
    ]

myMailbox : { address : Address String, signal : Signal String }
myMailbox = Signal.mailbox ""
