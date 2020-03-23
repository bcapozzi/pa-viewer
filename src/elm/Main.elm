module Main exposing (..)

import Browser exposing (..)
import Csv exposing (..)
import Html exposing (..)
import Html.Attributes exposing (checked, class, id, placeholder, src, style, title, type_, value, width)
import Html.Events exposing (..)
import Json.Decode as JD
import Ports exposing (CSVPortData, fileContentRead, fileSelected)
import Set exposing (..)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Msg
    = CSVSelected
    | CSVParse CSVPortData
    | Change String
    | ToggleSelection String


type alias CSVFile =
    { contents : String
    , filename : String
    }


type alias Model =
    { csvFile : Maybe CSVFile
    , csvData : Csv
    , personSelected : String
    , personOptions : List String
    , personToFocusOn : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing (Csv [] [ [] ]) "" [] "", Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    fileContentRead CSVParse


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CSVSelected ->
            ( model, fileSelected "CSVInput" )

        CSVParse data ->
            let
                newCSVFile =
                    { contents = data.contents
                    , filename = data.filename
                    }
            in
            case Csv.parse newCSVFile.contents of
                results ->
                    ( { model | csvFile = Just newCSVFile, csvData = results }, Cmd.none )

        Change content ->
            ( { model | personSelected = content }, Cmd.none )

        ToggleSelection name ->
            if String.length model.personToFocusOn > 0 then
                ( { model | personToFocusOn = "" }, Cmd.none )
            else
                ( { model | personToFocusOn = name }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "FileWrapper" ]
        [ input
            [ type_ "file"
            , id "CSVInput"
            , on "change"
                (JD.succeed CSVSelected)
            ]
            []
        , csvView model.csvFile model.csvData
        , button [] [ text "Press me" ]
        , input [ placeholder "Select person", value model.personSelected, onInput Change ] []
        , div [] [ text model.personSelected ]
        , viewPersonOptions model
        , h2 [] [ text model.personToFocusOn ]
        ]


viewPersonOptions : Model -> Html Msg
viewPersonOptions model =
    let
        options =
            findOptionsGiven model.personSelected model.csvData
    in
    div [] (List.map (\x -> renderOption x) options)


extractName : List String -> String
extractName record =
    let
        temp1 =
            List.drop 2 record
    in
    case temp1 of
        first :: rest ->
            first

        [] ->
            ""


findOptionsGiven : String -> Csv -> List String
findOptionsGiven selector csvData =
    let
        names =
            List.map (\r -> extractName r) csvData.records

        matching =
            Set.fromList (List.filter (\x -> String.startsWith selector x) names)
    in
    Set.toList matching


renderOption item =
    div []
        [ label []
            [ input [ type_ "checkbox", onClick (ToggleSelection item) ] [], text item ]
        , br [] []
        ]


csvView : Maybe CSVFile -> Csv -> Html Msg
csvView file csvData =
    case file of
        Just i ->
            csvTable csvData

        Nothing ->
            "Errors: " |> text


csvTable : Csv -> Html Msg
csvTable data =
    div []
        [ table []
            (tr []
                (List.map (\h -> th [] [ text h ]) data.headers)
                :: List.map (\r -> tr [] (List.map (\c -> td [] [ text c ]) r)) data.records
            )
        ]
