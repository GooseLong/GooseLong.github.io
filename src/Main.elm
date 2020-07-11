port module Main exposing (main)

import Browser
import Browser.Dom exposing (Element)
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Story exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { story : Story
    , current : StoryletID
    , orientation : Orientation
    }


init : { width : Int, height : Int } -> ( Model, Cmd Msg )
init windowSize =
    ( { story = Loading
      , current = StoryletID 1
      , orientation = .orientation <| classifyDevice windowSize
      }
    , Cmd.map StoryMsg <| Http.get { url = "assets/story.json", expect = Http.expectJson gotStory storyDecoder }
    )



-- SUBSCRIPTIONS / PORTS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\w h -> WindowResized w h)


port audioControl : String -> Cmd msg



--UPDATE


type Msg
    = WindowResized Int Int
    | StoryMsg Story.Msg



--    | ButtonPush


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResized w h ->
            ( { model | orientation = .orientation <| classifyDevice { width = w, height = h } }, Cmd.none )

        StoryMsg storyMsg ->
            case storyMsg of
                LoadedStory storylets ->
                    ( { model | story = Loaded storylets }, Cmd.none )

                ErrorLoadingStory error ->
                    ( { model | story = Error error }, Cmd.none )

                OptionClicked storyletid ->
                    ( { model | current = storyletid }, audioControl "pew" )



-- VIEW


view model =
    viewStorylet model.current model.orientation model.story
        |> Element.map StoryMsg
        |> layout []



-- character image (alt -> character's name)
-- their dialogue in a dialogue box
-- player's dialogue choices in a list of buttons
-- player chooses a dialogue option:
-- character image
--
