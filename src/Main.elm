module Main exposing (main)

import Browser
import Browser.Dom exposing (Element)
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : { width : Int, height : Int } -> ( Model, Cmd msg )
init windowSize =
    ( { character = CharacterA
      , orientation = .orientation <| classifyDevice windowSize
      }
    , Cmd.none
    )



-- MODEL


type alias Model =
    { character : Character
    , orientation : Orientation
    }


type Character
    = CharacterA
    | CharacterB



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\w h -> WindowResized w h)



--UPDATE


type Msg
    = WindowResized Int Int



--    | ButtonPush


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResized w h ->
            ( { model | orientation = .orientation <| classifyDevice { width = w, height = h } }, Cmd.none )



-- VIEW


view model =
    case model.orientation of
        Portrait ->
            layout [] <|
                column []
                    [ image [ width <| px 100 ] { src = "assets/chippy.png", description = "Chiptune" }
                    , paragraph [ Background.color <| rgb 0.2 0.3 0.99 ] [ text "testing" ]
                    , column []
                        [ Input.button [] { onPress = Nothing, label = text "test1" }
                        , Input.button [] { onPress = Nothing, label = text "test2" }
                        , Input.button [] { onPress = Nothing, label = text "test3" }
                        ]
                    ]

        Landscape ->
            layout [] <| row [] []



-- character image (alt -> character's name)
-- their dialogue in a dialogue box
-- player's dialogue choices in a list of buttons
-- player chooses a dialogue option:
-- character image
--
