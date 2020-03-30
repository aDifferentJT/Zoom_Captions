module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Browser
import Delay
import Html
import Http
import Random


type alias Model =
    { url : String
    , seq : Int
    , manualCaption : String
    }


type Msg
    = NOP
    | RunCmd (Cmd Msg)
    | ChangeURL String
    | SendCaption String
    | SendCaptionSeq Int Int String
    | ManualCaption String


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
    ( { url = ""
      , seq = 0
      , manualCaption = ""
      }
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    { title = "Zoom Captions"
    , body =
        [ CDN.stylesheet
        , Grid.container []
            [ Grid.row []
                [ Grid.col [] [ Html.text "URL:" ]
                , Grid.col [] [ Input.text [ Input.value model.url, Input.onInput ChangeURL ] ]
                ]
            , Grid.row []
                [ Grid.col [] [ Html.text "Manual Caption:" ]
                , Grid.col [] [ Input.text [ Input.value model.manualCaption, Input.onInput ManualCaption ] ]
                , Grid.col [] [ Button.button [ Button.onClick (SendCaption model.manualCaption) ] [ Html.text "Send" ] ]
                ]
            ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )

        RunCmd cmd ->
            ( model, cmd )

        ChangeURL url ->
            ( { model | url = url }
            , Cmd.none
            )

        SendCaption caption ->
            update (SendCaptionSeq 100 model.seq caption) { model | seq = model.seq + 1 }

        SendCaptionSeq randomDelay seq caption ->
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
                                        SendCaptionSeq (randomDelay * 2) seq caption
                                )
                                (Random.float 0 (toFloat randomDelay))
                            )
            in
            ( model
            , Http.post
                { url = model.url ++ "&seq=" ++ String.fromInt seq ++ "&lang=en-GB"
                , body = Http.stringBody "text/plain" caption
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
