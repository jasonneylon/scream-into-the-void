module Main where
import Graphics.Collage exposing (..)
import Graphics.Element exposing (image, fittedImage, croppedImage, Element, centered)
import Color exposing (red, black)
import Window
import Time exposing (..)
import Text exposing (color, fromString)

main : Signal Element
main = 
  Signal.map2 renderScene Window.dimensions (every (50 * millisecond))

renderScene: (Int, Int) -> Float -> Element
renderScene (width, height) time =
  let 
    angle = (toFloat ((round (6 * inSeconds time)) % 360))
  in
    collage width height
        [ 
           rect (toFloat width) (toFloat height) |> filled black |> move(0, 0),
           rotate (degrees angle) (toForm (image (width * 2) (height * 2) "/images/void_compressed.jpg")),
           rotate (degrees 20) (toForm (centered (color red (fromString ("Scream into the void : " ++ (toString angle))))))
        ]
