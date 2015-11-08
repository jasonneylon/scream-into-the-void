import Graphics.Collage exposing (..)
import Graphics.Element exposing (image, fittedImage, croppedImage, Element, centered, show, middle, container)
import Color exposing (red, black, white)
import Window
import Time exposing (..)
import Text exposing (color, fromString)
import Keyboard exposing (..)
import Html exposing (Html, fromElement, Attribute, text, toElement, div, input, h1, h2, node, button, a)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Signal exposing (Address, mailbox)

--http://screamintothevoid.com/

messageMailbox : { address : Address String, signal : Signal String }
messageMailbox = Signal.mailbox ""

main : Signal Html
main = 
  Signal.map4 renderTheVoid messageMailbox.signal (every (50 * millisecond)) (timestamp Keyboard.enter) Window.dimensions

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
    visibleInputStyles = 
      [
          ("display", "block")
        , ("width", "600px")
        , ("height", "100px")
        , ("font-size", "32pt")
      ]
    inputStyles = if show then [("display", "none")] else visibleInputStyles
  in
    div [style 
          [
            ("position", "absolute")
          , ("z-index", "100")
          , ("width", "100%")
          , ("height", "100%")
          , ("display", "flex")
          , ("justify-content", "center")
          , ("align-items", "center")
          ]
        ]
    [
      input [ style inputStyles
            , placeholder "Type your feelings here"
            , value message
            , on "input" targetValue (Signal.message messageMailbox.address)] 
          []
    --, button [style []] [Html.text "Scream!"]
    , div [style 
            [
              ("position", "absolute")
            , ("bottom", "0")
            , ("right", "0")
            , ("background-color", "azure")
            , ("opacity", "0.7")
            ]
          ] 
          [a [href "http://screamintothevoid.com/"] [Html.text "An Elm powered homage to http://screamintothevoid.com/"]]
    ]

css : String -> Html
css path =
  node "link" [ rel "stylesheet", href path ] []

renderTheVoid: String -> Float -> (Float, a) -> (Int, Int) -> Html
renderTheVoid message time (enterPressed, _) (width, height) =
  let 
    voidAngle = (toFloat ((round (6 * inSeconds time)) % 360))
    timeSinceEnterPressed = round ((time - enterPressed) / 100)
    shrinkingHeight = toFloat (72 - (3 * timeSinceEnterPressed))
    spiningMessageAngle = toFloat (60 + (24 * timeSinceEnterPressed))
    showInput = (timeSinceEnterPressed < 40)
    spiningMessage = if showInput then message else ""
  in
    div [] 
    [
      css "https://fonts.googleapis.com/css?family=Creepster"
    , heading
    , inputArea message showInput
    , fromElement (collage width height
      [ 
        rect (toFloat width) (toFloat height) |> filled black |> move(0, 0)
      , renderOuterVoid voidAngle width height
      , renderSpiningMessage spiningMessage spiningMessageAngle shrinkingHeight
      ])
    ]

renderOuterVoid : Float -> Int -> Int -> Form
renderOuterVoid voidAngle width height =
  rotate (degrees voidAngle) (toForm (image (width * 2) (height * 2) "./images/void_compressed.jpg"))

renderSpiningMessage : String -> Float -> Float -> Form
renderSpiningMessage message angle spinningHeight = 
  rotate (degrees angle) (toForm (centered (Text.height spinningHeight (color white (fromString message)))))

port shoutOut : Signal String
port shoutOut =
    (Signal.map2 signalMessageOnEnter Keyboard.enter messageMailbox.signal)

signalMessageOnEnter : Bool -> String -> String
signalMessageOnEnter enterPressed message =
  if enterPressed == True then message else ""
