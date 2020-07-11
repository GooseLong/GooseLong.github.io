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
import Json.Decode exposing (Decoder, field, int, list, string)



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


type Story
    = Loading
    | Loaded (List Storylet)
    | Error Http.Error


type StoryletID
    = StoryletID Int


type alias Storylet =
    { id : StoryletID
    , character : Character
    , paragraph : String
    , options : List StoryletID
    }


type Character
    = Chippy


characterFromString : String -> Decoder Character
characterFromString string =
    case string of
        "chippy" ->
            Json.Decode.succeed Chippy

        _ ->
            Json.Decode.fail ("Invalid character: " ++ string)


characterDecoder : Decoder Character
characterDecoder =
    string |> Json.Decode.andThen characterFromString


optionsDecoder : Decoder (List StoryletID)
optionsDecoder =
    list <| Json.Decode.map StoryletID int


storyletDecoder : Decoder Storylet
storyletDecoder =
    Json.Decode.map4 Storylet
        (field "id" <| Json.Decode.map StoryletID int)
        (field "character" characterDecoder)
        (field "paragraph" string)
        (field "options" optionsDecoder)


storyDecoder : Decoder (List Storylet)
storyDecoder =
    field "story" <| list storyletDecoder


gotStory : Result Http.Error (List Storylet) -> Msg
gotStory result =
    case result of
        Ok storylets ->
            LoadedStory storylets

        Err error ->
            ErrorLoadingStory error


init : { width : Int, height : Int } -> ( Model, Cmd Msg )
init windowSize =
    ( { story = Loading
      , current = StoryletID 1
      , orientation = .orientation <| classifyDevice windowSize
      }
    , Http.get { url = "/assets/story.json", expect = Http.expectJson gotStory storyDecoder }
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\w h -> WindowResized w h)



--UPDATE


type Msg
    = WindowResized Int Int
    | LoadedStory (List Storylet)
    | ErrorLoadingStory Http.Error



--    | ButtonPush


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResized w h ->
            ( { model | orientation = .orientation <| classifyDevice { width = w, height = h } }, Cmd.none )

        LoadedStory storylets ->
            ( { model | story = Loaded storylets }, Cmd.none )

        ErrorLoadingStory error ->
            ( { model | story = Error error }, Cmd.none )



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
