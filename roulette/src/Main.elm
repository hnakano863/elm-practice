module Main exposing (main)

import Browser
import Html exposing (Html, text, div , button )
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List.Extra as ListEx
import Process exposing(sleep)
import Random
import Task exposing (perform)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    List Drum


type alias Drum =
    { face : Int
    , isRolling : Bool
    }


init : () -> ( Model , Cmd Msg )
init _ =
    ( []
    , Cmd.none
    )


newDrum : Drum
newDrum =
    Drum 1 False



-- UPDATE


type Msg
    = DrumAdded
    | DrumDeleted Int
    | Rolling Int DrumMsg
    | RollAll


type DrumMsg
    = Roll
    | NewFace Int
    | Next ()
    | Stop


update : Msg -> Model -> ( Model , Cmd Msg )
update msg model =
    case msg of
        DrumAdded ->
            ( newDrum :: model
            , Cmd.none
            )

        DrumDeleted idx ->
            ( ListEx.removeAt idx model
            , Cmd.none
            )

        Rolling idx dmsg ->
            ( ListEx.updateAt idx (updateDrum dmsg) model
            , drumCmd idx dmsg model
            )

        RollAll ->
            ( List.map (updateDrum Roll) model
            , rollAll model
            )


rollAll : Model -> Cmd Msg
rollAll model =
    Cmd.batch
        <| List.map (\i -> drumCmd i Roll model)
        <| List.indexedMap always model


updateDrum : DrumMsg -> Drum -> Drum
updateDrum dmsg drum =
    case dmsg of
        NewFace newFace ->
            { drum | face = newFace }

        Roll ->
            { drum | isRolling = True }

        Stop ->
            { drum | isRolling = False }

        _ ->
            drum


drumCmd : Int -> DrumMsg -> Model -> Cmd Msg
drumCmd idx dmsg model =
    case dmsg of
        Roll ->
            Random.generate (( Rolling idx ) << NewFace) ( Random.int 1 9 )

        NewFace _ ->
            if isRollingAt idx model then
                Task.perform (( Rolling idx ) << Next) (sleep 20)
            else
                Cmd.none

        Next _ ->
            drumCmd idx Roll model

        Stop ->
            Cmd.none


isRollingAt : Int -> List Drum -> Bool
isRollingAt idx drums =
    case ListEx.getAt idx drums of
        Just drum ->
            drum.isRolling

        Nothing ->
            False



-- SUBSCRIPTIONS


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] ( List.reverse <| List.indexedMap viewDrum model )
        , viewRollButton model
        , viewAddButton model
        ]


viewDrum : Int -> Drum -> Html Msg
viewDrum idx drum =
    div [ style "display" "inline-block" ]
        [ div [] [ text ( String.fromInt drum.face ) ]
        , div [] [ button [ onClick (Rolling idx Stop) ] [ text "STOP" ] ]
        , div [] [ button [ onClick (DrumDeleted idx) ] [ text "DELETE" ] ]
        ]


viewRollButton : Model -> Html Msg
viewRollButton model =
    if List.any .isRolling model then
        div [] [ button [] [ text "Rolling..." ] ]
    else
        div [] [ button [ onClick RollAll ] [ text "ROLL" ] ]


viewAddButton : Model -> Html Msg
viewAddButton model =
    if List.any .isRolling model then
        div [] []
    else
        div [] [ button [ onClick DrumAdded ] [ text "ADD" ] ]
