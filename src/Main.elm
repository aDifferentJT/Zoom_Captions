module Main exposing (..)

import Basics.Extra
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Bootstrap.Navbar as Navbar
import Bootstrap.Table as Table
import Browser
import Browser.Events
import Debug
import Delay
import File
import File.Select
import Html
import Html.Attributes
import Html.Events
import Html.Parser
import Http
import Json.Decode
import List.Extra as List
import Maybe.Extra as Maybe
import Random
import Task


logId : a -> a
logId x =
    Debug.log (Debug.toString x) x


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


lookup : a -> List ( a, b ) -> Maybe b
lookup x yzs_ =
    case yzs_ of
        [] ->
            Nothing

        ( y, z ) :: yzs ->
            if x == y then
                Just z

            else
                lookup x yzs


takeDropUntil : (a -> Bool) -> List a -> ( List a, List a )
takeDropUntil f xs =
    case xs of
        [] ->
            ( [], [] )

        y :: ys ->
            if f y then
                ( [], xs )

            else
                Tuple.mapFirst ((::) y) (takeDropUntil f ys)


findByAttribute : Html.Parser.Attribute -> List Html.Parser.Node -> Maybe Html.Parser.Node
findByAttribute attr =
    Maybe.orList
        << List.map
            (\node ->
                case node of
                    Html.Parser.Element tag attrs children ->
                        if List.member attr attrs then
                            Just node

                        else
                            findByAttribute attr children

                    _ ->
                        Nothing
            )


type alias PsalmVerse =
    ( List String, List String )


type alias Psalm =
    List PsalmVerse


getFirstHalfOfPsalmVerse : List Html.Parser.Node -> ( List Html.Parser.Node, List Html.Parser.Node )
getFirstHalfOfPsalmVerse =
    takeDropUntil
        (\node ->
            case node of
                Html.Parser.Element tag attrs children ->
                    attrs |> lookup "class" |> Maybe.withDefault "" |> String.split " " |> List.member "indent1"

                _ ->
                    False
        )


getSecondHalfOfPsalmVerse : List Html.Parser.Node -> ( List Html.Parser.Node, List Html.Parser.Node )
getSecondHalfOfPsalmVerse =
    takeDropUntil
        (\node ->
            case node of
                Html.Parser.Element tag attrs children ->
                    attrs |> lookup "class" |> Maybe.withDefault "" |> String.split " " |> List.member "ve1"

                _ ->
                    False
        )


extractPsalmText : List Html.Parser.Node -> List String
extractPsalmText =
    List.map
        (\node ->
            case node of
                Html.Parser.Element tag attrs children ->
                    children
                        |> List.map
                            (\child ->
                                case child of
                                    Html.Parser.Text s ->
                                        s

                                    _ ->
                                        ""
                            )
                        |> String.join "\n"

                _ ->
                    ""
        )


parsePsalmVerses : List Html.Parser.Node -> Psalm
parsePsalmVerses nodes =
    if List.isEmpty nodes then
        []

    else
        let
            ( firstHalf, afterFirstHalf ) =
                getFirstHalfOfPsalmVerse nodes
        in
        let
            ( secondHalf, afterSecondHalf ) =
                getSecondHalfOfPsalmVerse afterFirstHalf
        in
        ( extractPsalmText firstHalf, extractPsalmText secondHalf ) :: parsePsalmVerses afterSecondHalf


getPsalmFile : Psalm -> List String
getPsalmFile =
    List.map
        (\( firstHalf, secondHalf ) ->
            String.join " " firstHalf ++ " : " ++ String.join " " secondHalf
        )


type alias Model =
    { navbarState : Navbar.State
    , importPsalmModalVisibility : Modal.Visibility
    , importPsalm : Int
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
    | ImportPsalmModalVisibility Modal.Visibility
    | ChangeImportPsalm Int
    | ChangeURL String
    | ChangeSeq Int
    | SendCaption Int Int String
    | ManualCaption String
    | OfferOpenFiles
    | PerformOpenFiles (List File.File)
    | CompleteOpenFiles (List ( String, String ))
    | ImportPsalm
    | PerformImportPsalm Int (List Html.Parser.Node)
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
      , importPsalmModalVisibility = Modal.hidden
      , importPsalm = 1
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
                , Navbar.dropdown
                    { id = "import"
                    , toggle = Navbar.dropdownToggle [] [ Html.text "Import" ]
                    , items = [ Navbar.dropdownItem [ Html.Events.onClick (ImportPsalmModalVisibility Modal.shown) ] [ Html.text "CW Psalm" ] ]
                    }
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
        , Modal.config (ImportPsalmModalVisibility Modal.hidden)
            |> Modal.withAnimation ImportPsalmModalVisibility
            |> Modal.body []
                [ Form.form []
                    [ Form.group []
                        [ Html.text "Psalm"
                        , Input.number
                            [ Input.value << String.fromInt <| model.importPsalm
                            , Input.onInput (Maybe.unwrap NOP ChangeImportPsalm << String.toInt)
                            ]
                        ]
                    ]
                ]
            |> Modal.footer []
                [ Button.button
                    [ Button.outlinePrimary
                    , Button.onClick <| Batch [ ImportPsalmModalVisibility Modal.hiddenAnimated, ImportPsalm ]
                    ]
                    [ Html.text "Import Psalm" ]
                ]
            |> Modal.view model.importPsalmModalVisibility
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

        ImportPsalmModalVisibility importPsalmModalVisibility ->
            ( { model | importPsalmModalVisibility = importPsalmModalVisibility }, Cmd.none )

        ChangeImportPsalm importPsalm ->
            ( { model | importPsalm = importPsalm }, Cmd.none )

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

        ImportPsalm ->
            ( model
            , Http.get
                { url = "https://cors-anywhere.herokuapp.com/https://www.churchofengland.org/prayer-and-worship/worship-texts-and-resources/common-worship/common-material/psalter/psalm-" ++ String.fromInt model.importPsalm
                , expect =
                    Http.expectString
                        (Result.toMaybe
                            >> Maybe.andThen (String.replace "<!DOCTYPE html>" "" >> Html.Parser.run >> Result.toMaybe)
                            >> Maybe.unwrap (ManualCaption "Failed") (PerformImportPsalm model.importPsalm)
                        )
                }
            )

        PerformImportPsalm number html ->
            let
                file =
                    case logId <| findByAttribute ( "class", "cw" ) html of
                        Just (Html.Parser.Element _ _ children) ->
                            children
                                |> parsePsalmVerses
                                |> getPsalmFile

                        _ ->
                            []
            in
            ( { model
                | files =
                    model.files
                        ++ [ ( "Psalm " ++ String.fromInt number
                             , file
                             )
                           ]
                , manualCaption = "Importing"
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
        , Modal.subscriptions model.importPsalmModalVisibility ImportPsalmModalVisibility
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
