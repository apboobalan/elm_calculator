module Calculator exposing (..)

import Html exposing(..)
import Html.Attributes exposing(..)
import Html.Events exposing(onClick)

--MODEL

type Mode = Add | Sub | Mul | Div

type alias Model =
        { numberDisplay : Int
        , mode : Maybe Mode
        , modeAppliedFunction : Maybe (Int -> Int)
        }

initialModel : Model
initialModel = 
        { numberDisplay = 0
        , mode = Nothing
        , modeAppliedFunction = Nothing
        }

addMode : Int -> Int -> Int
addMode a b = a + b

subMode : Int -> Int -> Int
subMode a b = a - b

mulMode : Int -> Int -> Int
mulMode a b = a * b

divMode : Int -> Int -> Int
divMode a b = a // b

equalMode : Int -> Int
equalMode a = a

setAppliedMode : Mode -> Int -> (Int -> Int)
setAppliedMode mode value =
        case mode of
                Add -> addMode value
                Sub -> subMode value
                Mul -> mulMode value
                Div -> divMode value


--UPDATES
type Msg = 
        DisplayNumber Int
        | ChangeMode Mode
        | ShowResult
        | Clear


update : Msg -> Model -> Model
update msg model=
        case msg of
                Clear -> initialModel
                DisplayNumber value ->
                        { model | numberDisplay = (model.numberDisplay * 10) + value }

                ChangeMode mode ->
                        let
                            appliedMode =
                                    case model.modeAppliedFunction of
                                            Nothing -> setAppliedMode mode model.numberDisplay
                                            Just appliedMode -> setAppliedMode mode (appliedMode model.numberDisplay)
                        in
                        { model | modeAppliedFunction = Just appliedMode
                        , numberDisplay = 0, mode = Just mode}
                ShowResult ->
                        let
                            appliedMode =
                                    case model.modeAppliedFunction of
                                            Nothing -> equalMode
                                            Just fun -> fun
                        in
                        { model | numberDisplay = appliedMode model.numberDisplay
                        , mode = Nothing}


--VIEW

viewNumberButton : Int -> Html Msg
viewNumberButton value =
        div [class "inline-block w-1/5 text-center"][
        div [class "inline-block p-2 m-2 h-16 w-16 bg-blue-400 rounded-full shadow-lg text-center text-3xl text-white cursor-pointer"
        , onClick (DisplayNumber value)][text (toString value)]]

viewModeButton : (Mode, String) -> Html Msg
viewModeButton (mode, value) =
        div [class "inline-block w-1/4 text-center"][
        div [class "inline-block p-2 m-2 h-16 w-16 bg-orange-500 rounded-full shadow-lg text-center text-3xl text-white font-extrabold cursor-pointer"
        , onClick (ChangeMode mode)][text value]]
        
viewNumberButtons : List (Html Msg)
viewNumberButtons = 
        List.range 0 9
        |> List.reverse
        |> List.map viewNumberButton

viewModeButtons : List (Html Msg)
viewModeButtons = 
        [(Add,"+"), (Sub,"-"), (Mul,"*"), (Div,"/")]
        |> List.map viewModeButton

viewMode : Maybe Mode -> Html Msg
viewMode mode =
        case mode of
            Just displayMode ->
                    let
                        displayValue =
                                case displayMode of
                                        Add -> "+"
                                        Sub -> "-"
                                        Mul -> "*"
                                        Div -> "/"
                    in
                    div [class "block h-8 pr-8 flex justify-end text-2xl text-grey-800"][text displayValue]
            Nothing ->
                    div [class "block h-8"][text ""]

viewCalculator : Model -> Html Msg
viewCalculator model =
        div [class "container bg-indigo-800 mx-auto p-8 mt-20 mb-20 pt-16 pb-16 shadow-lg rounded-lg"][
          div [class "flex justify-center text-white p-4 text-4xl font-extrabold"] [text "ELM CALC"]
        , div [class "rounded-lg mx-auto bg-white shadow-lg p-8 w-1/2"]
        [ div [class "block m-2 p-4 bg-gray-200 h-14 text-4xl rounded-lg flex justify-end cursor-pointer"][text (toString model.numberDisplay)]
        , viewMode model.mode
        , div [class "block"] viewNumberButtons
        , div [class "block"] viewModeButtons
        , div [class "inline-block w-1/2 text-center p-4"] [
         button [class "inline-block w-full p-4 m-4 bg-red-500 shadow-lg text-white text-3xl font-sans font-bold rounded-lg", onClick ShowResult] [text "="]
         ]
         , div [class "inline-block w-1/2 text-center p-4"] [
         button [class "inline-block w-full p-4 m-4 bg-red-500 shadow-lg text-white text-3xl font-sans font-bold rounded-lg", onClick Clear][text "C"]
        ]]
        ]

main : Program Never Model Msg
main =
        Html.beginnerProgram 
        { model = initialModel
        , update = update
        , view = viewCalculator
        }
