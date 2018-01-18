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
    combineHeadlessAndHtmlWithChannel prog1 prog2 |> Html.program



-- Ideas


ideas =
    True



-- This update function is generic and can be re-bound as a re-usable function within
-- any update function using it.
--update : Callbacks model msg -> Msg -> model -> ( model, Cmd msg )
-- This api call takes a tagger to the parent message type.
--invokeDelete : String -> (Msg -> msg) -> String -> Cmd msg


type alias Prog1 model msg snd recv =
    { init : ( model, Cmd msg )

    -- A send function is passed in that produces Cmds of the right type.
    , update : msg -> (snd -> Cmd msg) -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg

    -- changes always follow the (model, Cmd) pattern.
    , receive : recv -> model -> ( model, Cmd msg )
    }



-- Want to pass in APIs like this.


type alias Api msg =
    { time : Time -> Cmd msg
    }



-- Parent message type might have cases for the messages to.
-- type Msg a b
--     = AMsg a
--     | BMsg b
--     | AtoB s
--     | BtoA r


moreIdeas =
    Nothing



-- Program convolution


swap : ( a, b ) -> ( b, a )
swap ( a, b ) =
    ( b, a )


type alias HeadlessProgramWithChannel model msg snd recv =
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg, Maybe snd )
    , subscriptions : model -> Sub msg
    , receive : recv -> model -> model
    }


type alias HtmlProgramWithChannel model msg snd recv =
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg, Maybe snd )
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


{-| Combines a headless program with a
-}
combineHeadlessAndHtmlWithChannel :
    HeadlessProgramWithChannel modela msga send recv
    -> HtmlProgramWithChannel modelb msgb recv send
    -> HtmlProgram ( modela, modelb ) (Msg msga msgb)
combineHeadlessAndHtmlWithChannel progA progB =
    { init = init progA progB
    , update = update progA progB
    , subscriptions = subscriptions progA progB
    , view = view progB
    }


combineHeadlessWithChannel :
    HeadlessProgramWithChannel modela msga send recv
    -> HeadlessProgramWithChannel modelb msgb recv send
    -> HeadlessProgram ( modela, modelb ) (Msg msga msgb)
combineHeadlessWithChannel progA progB =
    { init = init progA progB
    , update = update progA progB
    , subscriptions = subscriptions progA progB
    }


{-| Combines the init fields of two programs.
-}
init :
    { a | init : ( modela, Cmd msga ) }
    -> { b | init : ( modelb, Cmd msgb ) }
    -> ( ( modela, modelb ), Cmd (Msg msga msgb) )
init progA progB =
    let
        modelA =
            Tuple.first progA.init

        modelB =
            Tuple.first progB.init

        cmdA =
            Tuple.second progA.init

        cmbB =
            Tuple.second progB.init
    in
        ( ( modelA, modelB )
        , Cmd.batch
            [ Cmd.map AMsg cmdA
            , Cmd.map BMsg cmbB
            ]
        )


{-| Combines the update functions of two programs, with receive channels.
-}
update :
    { a | receive : recv -> modela -> modela, update : msga -> modela -> ( modela, Cmd msga, Maybe snd ) }
    -> { b | receive : snd -> modelb -> modelb, update : msgb -> modelb -> ( modelb, Cmd msgb, Maybe recv ) }
    -> Msg msga msgb
    -> ( modela, modelb )
    -> ( ( modela, modelb ), Cmd (Msg msga msgb) )
update progA progB msg model =
    let
        modelA =
            Tuple.first model

        modelB =
            Tuple.second model

        updateAndSend progA progB msg modelA modelB tagger =
            let
                ( newModel, cmds, maybeSend ) =
                    progA.update msg modelA
            in
                ( ( newModel
                  , case maybeSend of
                        Just send ->
                            progB.receive send modelB

                        Nothing ->
                            modelB
                  )
                , Cmd.map tagger cmds
                )
    in
        case msg of
            AMsg amsg ->
                updateAndSend progA progB amsg modelA modelB AMsg

            BMsg bmsg ->
                updateAndSend progB progA bmsg modelB modelA BMsg
                    |> Tuple.mapFirst swap


{-| Combines the subscriptions of two programs.
-}
subscriptions :
    { a | subscriptions : modela -> Sub msga }
    -> { b | subscriptions : modelb -> Sub msgb }
    -> ( modela, modelb )
    -> Sub (Msg msga msgb)
subscriptions progA progB model =
    let
        modelA =
            Tuple.first model

        modelB =
            Tuple.second model
    in
        Sub.batch
            [ progA.subscriptions modelA |> Sub.map AMsg
            , progB.subscriptions modelB |> Sub.map BMsg
            ]


{-| Lifts the view of one program as the view of a combined program.
-}
view :
    { b | view : modelb -> Html msgb }
    -> ( modela, modelb )
    -> Html (Msg msga msgb)
view progB model =
    let
        modelB =
            Tuple.second model
    in
        progB.view modelB |> Html.map BMsg
