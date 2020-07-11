module Main exposing (main)

import Browser
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html



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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResized w h ->
            ( { model | orientation = .orientation <| classifyDevice { width = w, height = h } }, Cmd.none )



-- VIEW


view model =
    Element.layout [] <| text "hello world"



-- character image (alt -> character's name)
-- their dialogue in a dialogue box
-- player's dialogue choices in a list of buttons
-- player chooses a dialogue option:
-- character image
--
