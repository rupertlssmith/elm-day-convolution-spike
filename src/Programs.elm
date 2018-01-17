module Programs exposing (..)

import Html exposing (Html)
import Time exposing (Time, every, second)


-- Example


type alias Model =
    { message : String
    }


prog1 : HeadlessProgramWithChannel {} String String Never
prog1 =
    { init = ( {}, Cmd.none )
    , subscriptions = \_ -> every second (\time -> (toString time))
    , update = \time -> \model -> ( model, Cmd.none, Just time )
    , receive = \_ -> \model -> model
    }


prog2 : HtmlProgramWithChannel Model Never Never String
prog2 =
    { init = ( { message = "" }, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , update = \_ -> \model -> ( model, Cmd.none, Nothing )
    , view = \model -> Html.text <| "message: " ++ model.message
    , receive = \message -> \model -> { model | message = message }
    }


main =
    combine prog1 prog2 |> Html.program



-- Program convolution


type alias HeadlessProgramWithChannel model msg send recv =
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg, Maybe send )
    , subscriptions : model -> Sub msg
    , receive : recv -> model -> model
    }


type alias HtmlProgramWithChannel model msg send recv =
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg, Maybe send )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    , receive : recv -> model -> model
    }


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


combine :
    HeadlessProgramWithChannel modela msga send recv
    -> HtmlProgramWithChannel modelb msgb recv send
    -> HtmlProgram ( modela, modelb ) (Msg msga msgb)
combine a b =
    { init =
        ( ( Tuple.first a.init, Tuple.first b.init )
        , Cmd.batch
            [ Cmd.map (\amsg -> AMsg amsg) <| Tuple.second a.init
            , Cmd.map (\bmsg -> BMsg bmsg) <| Tuple.second b.init
            ]
        )
    , update =
        \msg ->
            case msg of
                AMsg amsg ->
                    \model ->
                        let
                            ( newA, cmda, maybeSend ) =
                                a.update amsg (Tuple.first model)
                        in
                            ( ( newA
                              , case maybeSend of
                                    Just send ->
                                        b.receive send <| Tuple.second model

                                    Nothing ->
                                        Tuple.second model
                              )
                            , Cmd.map (\amsg -> AMsg amsg) cmda
                            )

                BMsg bmsg ->
                    \model ->
                        let
                            ( newB, cmdb, maybeSend ) =
                                b.update bmsg (Tuple.second model)
                        in
                            ( ( case maybeSend of
                                    Just send ->
                                        a.receive send <| Tuple.first model

                                    Nothing ->
                                        Tuple.first model
                              , newB
                              )
                            , Cmd.map (\bmsg -> BMsg bmsg) cmdb
                            )
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
