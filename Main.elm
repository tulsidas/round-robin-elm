module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String exposing (isEmpty)


type alias Model =
    { name : String
    , entrants : List String
    , round : Int
    }


initModel : Model
initModel =
    { name = ""
    , entrants = []
    , round = 0
    }


main : Program Never Model Msg
main =
    beginnerProgram { model = initModel, view = view, update = update }



-- UPDATE


type Msg
    = Name String
    | NewEntrant
    | ChauEntrant String
    | NextRound
    | Restart


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        NewEntrant ->
            if isEmpty model.name || List.member model.name model.entrants then
                model
            else
                { model | entrants = model.name :: model.entrants, name = "" }

        ChauEntrant name ->
            let
                newEntrants =
                    List.filter (\e -> e /= name) model.entrants
            in
                { model | entrants = newEntrants }

        NextRound ->
            if model.round == 0 then
                let
                    adjustedEntrants =
                        if List.length model.entrants % 2 == 1 then
                            " --- " :: model.entrants
                        else
                            model.entrants
                in
                    { model | entrants = adjustedEntrants, round = model.round + 1 }
            else
                { model | entrants = cycle model.entrants, round = model.round + 1 }

        Restart ->
            initModel



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container-fluid text-center" ]
        [ if model.round == 0 then
            entrantInputs model
          else if model.round >= List.length model.entrants then
            showFin
          else
            showRounds model
        ]


entrantInputs : Model -> Html Msg
entrantInputs model =
    div []
        [ div [ class "form-inline" ]
            [ div [ class "form-group" ]
                [ label [ for "nameInput" ]
                    [ span
                        [ class "glyphicon glyphicon-user", style [ ( "fontSize", "2em" ) ] ]
                        []
                    ]
                , input [ id "nameInput", class "form-control input-lg", placeholder " ? ", onInput Name, value model.name ] []
                ]
            , button [ class "btn btn-default btn-lg", onClick NewEntrant ]
                [ span
                    [ class "glyphicon glyphicon-plus" ]
                    []
                ]
            ]
        , div []
            [ model.entrants
                |> List.sort
                |> List.map showEntrant
                |> ul [ class "list-group" ]
            ]
        , button [ class "btn btn-default btn-lg", onClick NextRound, disabled (List.length model.entrants < 3) ]
            [ span
                [ class "glyphicon glyphicon-ok-sign" ]
                []
            ]
        ]


showEntrant : String -> Html Msg
showEntrant entrant =
    li [ class "list-group-item" ]
        [ text entrant
        , button
            [ class "btn btn-danger btn-xs", onClick (ChauEntrant entrant) ]
            [ span
                [ class "glyphicon glyphicon-remove" ]
                []
            ]
        ]


showRounds : Model -> Html Msg
showRounds model =
    let
        halfSize =
            List.length model.entrants // 2

        pairs =
            zip (List.take halfSize model.entrants) (List.drop halfSize model.entrants)
    in
        div []
            [ h1 [] [ text ("Round " ++ (toString model.round)) ]
            , pairs
                |> List.map showPair
                |> div [ class "entrants" ]
            , button [ class "btn btn-default btn-lg", onClick NextRound ]
                [ span
                    [ class "glyphicon glyphicon-play" ]
                    []
                ]
            ]


cycle : List String -> List String
cycle entrants =
    let
        halfSize =
            List.length entrants // 2

        firstHalf =
            List.take halfSize entrants

        secondHalf =
            List.drop halfSize entrants
    in
        List.take 1 firstHalf
            ++ List.take 1 secondHalf
            ++ List.take (halfSize - 2) (List.drop 1 firstHalf)
            ++ List.drop 1 secondHalf
            ++ List.drop (halfSize - 1) firstHalf


showPair : ( String, String ) -> Html msg
showPair pair =
    div [ class "entrant" ]
        [ text (Tuple.first pair)
        , text " vs "
        , text (Tuple.second pair)
        ]


showFin : Html Msg
showFin =
    div []
        [ h1 [] [ text "Fin" ]
        , button [ class "btn btn-default btn-lg", onClick Restart ]
            [ span
                [ class "glyphicon glyphicon-repeat" ]
                []
            ]
        ]


zip : List a -> List b -> List ( a, b )
zip =
    List.map2 (,)
