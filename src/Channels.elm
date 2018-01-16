module Channels exposing (..)

import Html exposing (Html)
import Time exposing (Time, every, second)


-- Example


type alias Model =
    { message : String
    }


prog1 : ProgramWithChannel {} String Never
prog1 =
    { commands = \_ -> Cmd.none
    , model = {}
    , subscriptions = \_ -> every second (\time -> ( {}, (toString time) ))
    , view = \model -> Html.text ""
    , receive = \message -> \model -> model
    }


prog2 : ProgramWithChannel Model Never String
prog2 =
    { commands = \model -> Cmd.none
    , model = { message = "" }
    , subscriptions = \model -> Sub.none
    , view = \model -> Html.text <| "message: " ++ model.message
    , receive = \message -> \model -> { model | message = message }
    }


main =
    tuple2 prog1 prog2 |> program



-- Program convolution


type alias ProgramWithChannel model send recv =
    { commands : model -> Cmd model
    , model : model
    , subscriptions : model -> Sub ( model, send )
    , view : model -> Html model
    , receive : recv -> model -> model
    }


type alias Program model =
    { commands : model -> Cmd model
    , model : model
    , subscriptions : model -> Sub model
    , view : model -> Html model
    }


program : Program model -> Platform.Program Never model model
program { commands, model, subscriptions, view } =
    Html.program
        { init = ( model, commands model )
        , subscriptions = subscriptions
        , update = \model _ -> ( model, commands model )
        , view = view
        }


tuple2 : ProgramWithChannel a sendA recvA -> ProgramWithChannel b recvA sendA -> Program ( a, b )
tuple2 a b =
    { commands =
        \( newA, newB ) ->
            Cmd.batch
                [ Cmd.map (\a -> ( a, newB )) (a.commands newA)
                , Cmd.map (\b -> ( newA, b )) (b.commands newB)
                ]
    , model = ( a.model, b.model )
    , subscriptions =
        \( newA, newB ) ->
            Sub.batch
                [ Sub.map (\( a, msg ) -> ( a, b.receive msg newB )) (a.subscriptions newA)
                , Sub.map (\( b, msg ) -> ( a.receive msg newA, b )) (b.subscriptions newB)
                ]
    , view =
        \( newA, newB ) ->
            Html.div
                []
                [ Html.map (\a -> ( a, newB )) (a.view newA)
                , Html.map (\b -> ( newA, b )) (b.view newB)
                ]
    }
