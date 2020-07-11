module Main exposing (main)

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
import Typewriter



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
    , typewriter : Typewriter.Model
    }


init : { width : Int, height : Int } -> ( Model, Cmd Msg )
init windowSize =
    let
        ( typewriter, twCmd ) =
            Typewriter.withWords [ "Loading . . ." ]
                |> Typewriter.iterations Typewriter.infinite
                |> Typewriter.init
    in
    ( { story = Loading
      , current = StoryletID 1
      , orientation = .orientation <| classifyDevice windowSize
      , typewriter = typewriter
      }
    , Cmd.batch
        [ Cmd.map TypewriterMsg twCmd
        , Cmd.map StoryMsg <| Http.get { url = "assets/story.json", expect = Http.expectJson gotStory storyDecoder }
        ]
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\w h -> WindowResized w h)



--UPDATE


type Msg
    = WindowResized Int Int
    | StoryMsg Story.Msg
    | TypewriterMsg Typewriter.Msg



--    | ButtonPush


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResized w h ->
            ( { model | orientation = .orientation <| classifyDevice { width = w, height = h } }, Cmd.none )

        StoryMsg storyMsg ->
            case storyMsg of
                LoadedStory storylets ->
                    let
                        story =
                            Loaded storylets

                        ( typewriter, twCmd ) =
                            Typewriter.withWords [ getParagraphFromId model.current story ]
                                |> Typewriter.iterations (Typewriter.times 1)
                                |> Typewriter.init
                    in
                    ( { model | story = story, typewriter = typewriter }, Cmd.map TypewriterMsg twCmd )

                ErrorLoadingStory error ->
                    ( { model | story = Error error }, Cmd.none )

                OptionClicked storyletid ->
                    let
                        ( typewriter, twCmd ) =
                            Typewriter.withWords [ getParagraphFromId storyletid model.story ]
                                |> Typewriter.iterations (Typewriter.times 1)
                                |> Typewriter.init
                    in
                    ( { model | current = storyletid, typewriter = typewriter }, Cmd.map TypewriterMsg twCmd )

        TypewriterMsg twMsg ->
            let
                ( typewriter, cmd ) =
                    Typewriter.update twMsg model.typewriter
            in
            ( { model | typewriter = typewriter }, Cmd.map TypewriterMsg cmd )



-- VIEW


view model =
    viewStorylet model.current model.orientation (Typewriter.view model.typewriter) model.story
        |> Element.map StoryMsg
        |> layout []



-- character image (alt -> character's name)
-- their dialogue in a dialogue box
-- player's dialogue choices in a list of buttons
-- player chooses a dialogue option:
-- character image
--
