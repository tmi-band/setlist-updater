port module Main exposing (..)

{-| TodoMVC implemented in Elm, using plain HTML and CSS for rendering.

This application is broken up into three key parts:

1. Model  - a full definition of the application's state
2. Update - a way to step the application state forward
3. View   - a way to visualize our application state with HTML

This clean division of concerns is a core part of Elm. You can read more about
this in <http://guide.elm-lang.org/architecture/index.html>
-}

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import Task


main : Program (Maybe Model) Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Elm • SetListMVC", body = [view model] }
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


port setStorage : Model -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ setStorage newModel, cmds ]
        )



-- MODEL


-- The full application state of our set list app.
type alias Model =
    { songs : List Song
    , field : String
    , uid : Int
    }


type alias Song =
    { title: String
    , artist: String
    , tunedDown: Bool
    , id: Int
    }


emptyModel : Model
emptyModel =
    { songs = []
    , field = ""
    , uid = 0
    }


newSong : String -> Int -> Song
newSong title id =
    { title = title
    , artist = ""
    , tunedDown = False
    , id = id
    }


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
    ( Maybe.withDefault emptyModel savedModel, Cmd.none )



-- UPDATE


{-| Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
-}
type Msg
    = NoOp
    | UpdateField String
    | Add
    | Delete Int



-- How we update our Model on a given Msg?
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)

        Add ->
            ( { model
                  | uid = model.uid + 1
                  , field = ""
                  , songs =
                      if String.isEmpty model.field then
                          model.songs
                      else
                          model.songs ++ [ newSong model.field model.uid ]
              }
            , Cmd.none
            )

        UpdateField str ->
            ( { model | field = str }
            , Cmd.none
            )

        Delete id ->
            ( { model | songs = List.filter (\t -> t.id /= id) model.songs }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "setlists-wrapper"
        , style "visibility" "hidden"
        ]
        [ section
            [ class "setlistapp" ]
            [ lazy viewInput model.field
            , lazy viewSongs model.songs
            , lazy viewControls model.songs
            ]
        , infoFooter
        ]


viewInput : String -> Html Msg
viewInput song =
    header
        [ class "header" ]
        [ h1 [] [ text "Songs" ]
        , input
            [ class "new-song"
            , placeholder "Song name"
            , autofocus True
            , value song
            , name "newSong"
            , onInput UpdateField
            , onEnter Add
            ]
            []
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)



-- VIEW ALL SONGS


viewSongs : List Song -> Html Msg
viewSongs songs =
    let
        cssVisibility =
            if List.isEmpty songs then
                "hidden"
            else
                "visible"
    in
        section
            [ class "main"
            , style "visibility" cssVisibility
            ]
            [ Keyed.ul [ class "song-list" ] <|
                List.map viewKeyedSong songs
            ]



-- VIEW INDIVIDUAL SONGS


viewKeyedSong : Song -> ( String, Html Msg )
viewKeyedSong song =
    ( String.fromInt song.id, lazy viewSong song )


viewSong : Song -> Html Msg
viewSong song =
    li
        [ classList [ ] ]
        [ div
            [ class "view" ]
            [ button
                [ class "destroy"
                , onClick (Delete song.id)
                ]
                []
            ]
        , input
            [ class "view"
            , value song.title
            , name "title"
            , id ("song-" ++ String.fromInt song.id)
            ]
            []
        ]



-- VIEW CONTROLS AND FOOTER


viewControls : List Song -> Html Msg
viewControls songs =
    footer
        [ class "footer"
        , hidden (List.isEmpty songs)
        ]
        [ lazy viewControlsCount (List.length songs)
        ]


viewControlsCount : Int -> Html Msg
viewControlsCount songsLeft =
    let
        song_ =
            if songsLeft == 1 then
                " song"
            else
                " songs"
    in
        span
            [ class "song-count" ]
            [ strong [] [ text (String.fromInt songsLeft) ]
            , text (song_ ++ " left")
            ]


infoFooter : Html msg
infoFooter =
    footer [ class "info" ]
        [ p []
            [ text "Written by "
            , a [ href "https://github.com/evancz" ] [ text "Nobody Inparticular" ]
            ]
        ]
