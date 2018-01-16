module Programs exposing (..)

import Html exposing (Html)


main =
    Html.text "hello"


type alias HeadlessProgram model msg =
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    }


type alias HtmlProgram model msg =
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }


type Msg a b
    = AMsg a
    | BMsg b


tuple :
    HeadlessProgram modela msga
    -> HtmlProgram modelb msgb
    -> HtmlProgram ( modela, modelb ) (Msg msga msgb)
tuple a b =
    { init =
        ( ( Tuple.first a.init, Tuple.first b.init )
        , Cmd.batch
            [ Cmd.map (\amsg -> AMsg amsg) <| Tuple.second a.init
            , Cmd.map (\bmsg -> BMsg bmsg) <| Tuple.second b.init
            ]
        )
    , update = \msg -> \model -> ( model, Cmd.none )
    , subscriptions =
        \model ->
            Sub.batch
                [ Sub.map (\amsg -> AMsg amsg) <| a.subscriptions (Tuple.first model)
                , Sub.map (\bmsg -> BMsg bmsg) <| b.subscriptions (Tuple.second model)
                ]
    , view =
        \model ->
            b.view (Tuple.second model)
                |> Html.map (\bmsg -> BMsg bmsg)
    }
