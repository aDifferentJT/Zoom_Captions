module Main exposing (..)

import Basics.Extra
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Bootstrap.Table as Table
import Browser
import Browser.Events
import Delay
import File
import File.Select
import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import List.Extra as List
import Maybe.Extra as Maybe
import Random
import Task


transpose : Int -> List a -> List a
transpose n_ xs_ =
    case ( n_, xs_ ) of
        ( _, [] ) ->
            []

        ( 0, x1 :: x2 :: xs ) ->
            x2 :: x1 :: xs

        ( n, x :: xs ) ->
            x :: transpose (n - 1) xs


removeAt : Int -> List a -> List a
removeAt n_ xs_ =
    case ( n_, xs_ ) of
        ( _, [] ) ->
            []

        ( 0, _ :: xs ) ->
            xs

        ( n, x :: xs ) ->
            x :: removeAt (n - 1) xs


type alias Model =
    { navbarState : Navbar.State
    , url : String
    , seq : Int
    , manualCaption : String
    , files : List ( String, List String )
    , selectedFile : Maybe Int
    , captions : List String
    , selectedCaption : Maybe Int
    }


type Msg
    = NOP
    | Batch (List Msg)
    | RunCmd (Cmd Msg)
    | NavbarMsg Navbar.State
    | ChangeURL String
    | ChangeSeq Int
    | SendCaption Int Int String
    | ManualCaption String
    | OfferOpenFiles
    | PerformOpenFiles (List File.File)
    | CompleteOpenFiles (List ( String, String ))
    | SelectFile Int
    | MoveUpFile Int
    | MoveDownFile Int
    | RemoveFile Int
    | SelectCaption Int


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init () =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( { navbarState = navbarState
      , url = ""
      , seq = 0
      , manualCaption = ""
      , files = []
      , selectedFile = Nothing
      , captions = []
      , selectedCaption = Nothing
      }
    , navbarCmd
    )


view : Model -> Browser.Document Msg
view model =
    { title = "Zoom Captions"
    , body =
        [ CDN.stylesheet
        , Navbar.config NavbarMsg
            |> Navbar.withAnimation
            |> Navbar.brand [] [ Html.text "Zoom Captions" ]
            |> Navbar.items
                [ Navbar.itemLink [ Html.Events.onClick OfferOpenFiles ] [ Html.text "Open" ]
                ]
            |> Navbar.customItems
                [ Navbar.formItem []
                    [ Html.text "URL:"
                    , Input.text [ Input.value model.url, Input.onInput ChangeURL ]
                    ]
                , Navbar.formItem []
                    [ Html.text "Seq:"
                    , Input.number [ Input.value << String.fromInt <| model.seq, Input.onInput (Maybe.unwrap NOP ChangeSeq << String.toInt) ]
                    ]
                ]
            |> Navbar.view model.navbarState
        , Grid.container []
            [ Grid.row []
                [ Grid.col [ Col.sm ] [ Html.text "Manual Caption:" ]
                , Grid.col [] [ Input.text [ Input.value model.manualCaption, Input.onInput ManualCaption ] ]
                , Grid.col [] [ Button.button [ Button.onClick (Batch [ ChangeSeq (model.seq + 1), SendCaption 100 model.seq model.manualCaption ]) ] [ Html.text "Send" ] ]
                ]
            , Grid.row
                [ Row.attrs [ Html.Attributes.class "h-100" ] ]
                [ Grid.col []
                    [ Table.table
                        { options = []
                        , thead = Table.thead [] []
                        , tbody =
                            Table.tbody []
                                (List.indexedMap
                                    (\n ( name, captions ) ->
                                        Table.tr
                                            (if Just n == model.selectedFile then
                                                [ Table.rowActive ]

                                             else
                                                []
                                            )
                                            [ Table.td []
                                                [ Button.button
                                                    [ Button.onClick (SelectFile n) ]
                                                    [ Html.text name ]
                                                ]
                                            , Table.td []
                                                [ Button.button
                                                    [ Button.onClick (MoveUpFile n) ]
                                                    [ Html.text "▲" ]
                                                ]
                                            , Table.td []
                                                [ Button.button
                                                    [ Button.onClick (MoveDownFile n) ]
                                                    [ Html.text "▼" ]
                                                ]
                                            , Table.td []
                                                [ Button.button
                                                    [ Button.onClick (RemoveFile n) ]
                                                    [ Html.text "🗙" ]
                                                ]
                                            ]
                                    )
                                    model.files
                                )
                        }
                    ]
                , Grid.col []
                    [ Table.table
                        { options = []
                        , thead = Table.thead [] []
                        , tbody =
                            Table.tbody []
                                (List.indexedMap
                                    (\n caption ->
                                        Table.tr
                                            (if Just n == model.selectedCaption then
                                                [ Table.rowActive ]

                                             else
                                                []
                                            )
                                            [ Table.td []
                                                [ Button.button
                                                    [ Button.onClick (SelectCaption n) ]
                                                    [ Html.text caption ]
                                                ]
                                            ]
                                    )
                                    model.captions
                                )
                        }
                    ]
                ]
            ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )

        Batch ms ->
            ms
                |> List.foldr (\msg_ ( model_, cmds ) -> Tuple.mapSecond (Basics.Extra.flip (::) cmds) <| update msg_ model_) ( model, [] )
                |> Tuple.mapSecond Cmd.batch

        RunCmd cmd ->
            ( model, cmd )

        NavbarMsg navbarState ->
            ( { model | navbarState = navbarState }, Cmd.none )

        ChangeURL url ->
            ( { model | url = url }, Cmd.none )

        ChangeSeq seq ->
            ( { model | seq = seq }, Cmd.none )

        SendCaption randomDelay seq caption ->
            let
                retry =
                    if randomDelay >= 1000 then
                        NOP

                    else
                        RunCmd
                            (Random.generate
                                (\t ->
                                    RunCmd
                                        << Delay.after t Delay.Millisecond
                                    <|
                                        SendCaption (randomDelay * 2) seq caption
                                )
                                (Random.float 0 (toFloat randomDelay))
                            )
            in
            ( model
            , Http.post
                { url = model.url ++ "&seq=" ++ String.fromInt seq ++ "&lang=en-GB"
                , body = Http.stringBody "text/plain" <| caption ++ "\n"
                , expect =
                    Http.expectWhatever
                        (\res ->
                            case res of
                                Ok () ->
                                    NOP

                                Err (Http.BadUrl _) ->
                                    NOP

                                Err Http.Timeout ->
                                    retry

                                Err Http.NetworkError ->
                                    NOP

                                Err (Http.BadStatus _) ->
                                    retry

                                Err (Http.BadBody _) ->
                                    NOP
                        )
                }
            )

        ManualCaption manualCaption ->
            ( { model | manualCaption = manualCaption }
            , Cmd.none
            )

        OfferOpenFiles ->
            ( model, File.Select.files [ "text/plain" ] ((<<) PerformOpenFiles << (::)) )

        PerformOpenFiles fs ->
            ( model
            , Task.perform (CompleteOpenFiles << List.zip (List.map File.name fs)) (Task.sequence <| List.map File.toString <| fs)
            )

        CompleteOpenFiles fs ->
            ( { model
                | files =
                    model.files
                        ++ List.filterMap
                            (\( name, captions ) ->
                                case Json.Decode.decodeString (Json.Decode.list Json.Decode.string) captions of
                                    Ok captions_ ->
                                        Just ( name, captions_ )

                                    Err _ ->
                                        Nothing
                            )
                            fs
              }
            , Cmd.none
            )

        SelectFile n ->
            ( { model
                | captions = Maybe.unwrap [] Tuple.second <| List.getAt n <| model.files
                , selectedCaption = Nothing
              }
            , Cmd.none
            )

        MoveUpFile n ->
            ( { model | files = transpose (n - 1) model.files }, Cmd.none )

        MoveDownFile n ->
            ( { model | files = transpose n model.files }, Cmd.none )

        RemoveFile n ->
            ( { model | files = removeAt n model.files }, Cmd.none )

        SelectCaption n ->
            case List.getAt n model.captions of
                Just caption ->
                    update (SendCaption 100 model.seq caption) { model | seq = model.seq + 1, selectedCaption = Just n }

                Nothing ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Navbar.subscriptions model.navbarState NavbarMsg
        , Browser.Events.onKeyDown
            (Json.Decode.field "key" Json.Decode.string
                |> Json.Decode.andThen
                    (\key ->
                        case ( model.selectedCaption, key ) of
                            ( Just n, "ArrowUp" ) ->
                                Json.Decode.succeed <| SelectCaption (n - 1)

                            ( Just n, "ArrowDown" ) ->
                                Json.Decode.succeed <| SelectCaption (n + 1)

                            _ ->
                                Json.Decode.fail ""
                    )
            )
        ]
