port module Main exposing (..)

import Material
import Material.Button as Button
import Material.Options as Options
import Material.Textfield as Textfield
import Material.Snackbar as Snackbar
import Material.Icon as Icon
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Url
import Url.Parser as Parser exposing (Parser, (</>), (<?>))
import Url.Parser.Query as Query
import Task
import List.Extra as List
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard.Key as Key
import Json.Encode as E
import Http
import Json.Decode as D
import Json.Decode.Extra as D
import Url.Builder as Url


---- HTTP ----


type alias PlaceModel =
    { lon : Float
    , lat : Float
    , displayName : String
    }


placeDecoder : D.Decoder PlaceModel
placeDecoder =
    D.map3 PlaceModel
        (D.field "lon" D.parseFloat)
        (D.field "lat" D.parseFloat)
        (D.field "display_name" D.string)


geocodeUrl : String -> String
geocodeUrl q = 
    Url.crossOrigin "https://nominatim.openstreetmap.org" ["search"]
        [ Url.string "q" q
        , Url.string "format" "json"
        ]


geocode : String -> Cmd Msg
geocode q =
    Http.send Geocode <| Http.get (geocodeUrl q) (D.list placeDecoder)


reverseUrl : Float -> Float -> String
reverseUrl lon lat = 
    Url.crossOrigin "https://nominatim.openstreetmap.org" ["reverse"]
        [ Url.string "lon" <| String.fromFloat lon
        , Url.string "lat" <| String.fromFloat lat
        , Url.string "format" "json"
        ]


reverseGeocode : Maybe Float -> Maybe Float -> Cmd Msg
reverseGeocode maybeLon maybeLat =
    case maybeLon of
        Nothing ->
            Cmd.none
    
        Just lon ->
            case maybeLat of
                Nothing ->
                    Cmd.none
            
                Just lat ->
                    Http.send ReverseGeocode <| Http.get (reverseUrl lon lat) placeDecoder


httpErrorMessage : Http.Error -> String -> String
httpErrorMessage err base =
    case err of
        Http.BadUrl url ->
            "Ongeldige url bij " ++ base ++ " " ++ url

        Http.Timeout ->
            "Timeout bij " ++ base

        Http.NetworkError ->
            "Netwerkfout bij " ++ base

        Http.BadStatus response ->
            "Foutcode " ++ (String.fromInt response.status.code) ++ " bij " ++ base ++ ": " ++ response.status.message

        Http.BadPayload message _ ->
            "Datafout bij " ++ base ++ ": " ++ message


---- MODEL ----


type alias FieldModel =
    { typed : String
    , saved : String
    , focused : Bool
    , select : Bool
    , format : (String -> String)
    }


fieldValue : String -> Model -> String
fieldValue field model =
    (getField field model).saved


getField : String -> Model -> FieldModel
getField field model =
    Dict.get field model.fields |> Maybe.withDefault defaultFieldModel


typeField : String -> String -> Model -> Model
typeField field typed model =
    updateField field (Just typed) Nothing model


untypeField : String -> Model -> Model
untypeField field model =
    model
        |> typeField field (fieldValue field model)
        |> focusField field False


saveField : String -> String -> Model -> Model
saveField field saved model =
    updateField field Nothing (Just saved) model


updateField : String -> Maybe String -> Maybe String -> Model -> Model
updateField field maybeTyped maybeSaved model =
    { model | fields = model.fields |>
        Dict.update field (\maybeFieldModel ->
            case maybeFieldModel of
                Nothing ->
                    Nothing
            
                Just fieldModel ->
                    case maybeSaved of
                        Just saved ->
                            let
                                formatted =
                                    fieldModel.format saved
                            in
                            Just { fieldModel
                            | saved = formatted
                            , typed = formatted
                            , focused = False
                            }

                        Nothing ->
                            case maybeTyped of
                                Just typed ->
                                    Just { fieldModel | typed = typed }

                                Nothing ->
                                    Nothing
        )
    }


focusField : String -> Bool -> Model -> Model
focusField field focused model =
    { model | fields = model.fields |>
        Dict.update field (\maybeFieldModel ->
            case maybeFieldModel of
                Nothing ->
                    Nothing
            
                Just fieldModel ->
                    Just { fieldModel | focused = focused }
        )
    }


defaultFieldModel : FieldModel
defaultFieldModel =
    FieldModel "" "" False True <| \v -> v


floatFieldModel : FieldModel
floatFieldModel =
    FieldModel "" "" False False <| \input ->
        case (String.toFloat input) of
            Nothing ->
                ""
                
            Just float ->
                let
                    parts =
                        String.split "." input
                    
                    first =
                        List.head parts |> Maybe.withDefault ""
                    
                    last =
                        List.last parts |> Maybe.withDefault ""
                in
                if List.length parts == 2
                && String.length last > 5 then
                    let
                        fraction =
                            String.toFloat ("0." ++ last) |> Maybe.withDefault 0

                        decimals =
                            String.fromInt <| round (fraction * 100000)
                    in
                    first ++ "." ++ decimals
                
                else
                    input


type alias Model =
    { mdc : Material.Model Msg
    , url : Url.Url
    , key : Nav.Key
    , fields : Dict String FieldModel
    }


defaultModel : Url.Url -> Nav.Key -> Model
defaultModel url key =
    { mdc = Material.defaultModel
    , url = url
    , key = key
    , fields = Dict.fromList
        [ ("lon", floatFieldModel)
        , ("lat", floatFieldModel)
        , ("place", defaultFieldModel)
        ]
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( model, cmd) =
            route <| defaultModel url key
    in
    ( model, Cmd.batch
        [ Material.init Mdc
        , cmd
        ]
    )


---- ROUTES ----


queryFloat : String -> Query.Parser (Maybe Float)
queryFloat param =
    Query.custom param <| \stringList ->
        case stringList of
            [str] ->
                String.toFloat str

            _ ->
                Nothing


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Home    (Parser.top)
        , Parser.map Search  (Parser.s "search" <?> Query.string "q")
        , Parser.map Reverse (Parser.s "reverse" <?> queryFloat "lon" <?> queryFloat "lat")
        ]


type Route
    = Home
    | Search (Maybe String)
    | Reverse (Maybe Float) (Maybe Float)


route : Model -> ( Model, Cmd Msg )
route model =
    case Parser.parse routeParser model.url of
        Nothing ->
            ( model, Nav.pushUrl model.key "/" )

        Just route_ ->
            case route_ of
                Home ->
                    ( model, geocode "onze lieve vrouwetoren, amersfoort" )
            
                Search maybeQ ->
                    case maybeQ of
                        Nothing ->
                            toast "Geen zoekopdracht gevonden" model
                    
                        Just q ->
                            ( model, geocode q )
            
                Reverse maybeLon maybeLat ->
                    case maybeLon of
                        Nothing ->
                            toast "Geen geldige lengtegraad" model
                    
                        Just lon ->
                            case maybeLat of
                                Nothing ->
                                    toast "Geen geldige breedtegraad" model
                            
                                Just lat ->
                                    let
                                        lonString = String.fromFloat lon
                                        latString = String.fromFloat lat
                                    in
                                    ( model
                                        |> saveField "lon" lonString
                                        |> saveField "lat" latString
                                    , lonLatCmd lonString latString )


---- UPDATE ----


toast : String -> Model -> ( Model, Cmd Msg )
toast message model =
    let
        contents =
            Snackbar.toast Nothing message
        ( mdc, effects ) =
            Snackbar.add Mdc "my-snackbar" contents model.mdc
    in
    ( { model | mdc = mdc }, effects )


lonLatCmd : String -> String -> Cmd Msg
lonLatCmd lon lat =
    let
        maybeLon = String.toFloat lon
        maybeLat = String.toFloat lat
    in
    Cmd.batch
        [ mapFly maybeLon maybeLat
        , reverseGeocode maybeLon maybeLat
        ]


fieldCmd : String -> Model -> Cmd Msg
fieldCmd field model =
    let
        value_ = fieldValue field model
    in
    case field of
        "lon" ->
            lonLatCmd value_ (fieldValue "lat" model)
            
        "lat" ->
            lonLatCmd (fieldValue "lon" model) value_
            
        "place" ->
            geocode value_
        
        _ ->
            Cmd.none


type alias Coordinate =
    { lon : Float
    , lat : Float
    , there : Bool
    }


type Msg
    = Mdc (Material.Msg Msg)
    | NoOp
    | FieldFocus String
    | FieldKey String Int
    | FieldInput String String
    | FieldChange String String
    | MapCenter Coordinate
    | Geocode (Result Http.Error (List PlaceModel))
    | ReverseGeocode (Result Http.Error PlaceModel)
    | UrlChange Url.Url
    | UrlRequest Browser.UrlRequest
    

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdc msg_ ->
            Material.update Mdc msg_ model
        
        NoOp ->
            ( model, Cmd.none )

        FieldFocus field ->
            ( model |> focusField field True
            , if (getField field model).select
                then selectText <| "textfield-" ++ field ++ "-native"
                else Cmd.none )
        
        FieldKey field code ->
            let
                blur =
                    Task.attempt (\_ -> NoOp) <| Dom.blur ("textfield-" ++ field ++ "-native")
            in
            case (Key.fromCode code) of
                Key.Enter ->
                    ( model |> focusField field False, blur )
                
                Key.Escape ->
                    ( model |> untypeField field, blur )
                
                _ ->
                    ( model, Cmd.none )

        FieldInput field input ->
            ( model |> typeField field input, Cmd.none )

        FieldChange field input ->
            if input == "" then
                ( model |> untypeField field, Cmd.none )
            
            else
                let
                    newmodel = model
                        |> saveField field input
                in
                ( newmodel, fieldCmd field newmodel )

        MapCenter coordinate ->
            let
                pan = if coordinate.there
                    then Cmd.none
                    else mapPan (Just coordinate.lon) (Just coordinate.lat)
            in
            ( model
                |> saveField "lat" (String.fromFloat coordinate.lon)
                |> saveField "lon" (String.fromFloat coordinate.lat)
            , Cmd.batch
                [ pan
                , reverseGeocode (Just coordinate.lon) (Just coordinate.lat)
                ] )
                                
        Geocode result ->
            case result of
                Ok places ->
                    case (List.head places) of
                        Nothing ->
                            model |> toast "Niets gevonden"
                            
                        Just place ->
                            ( model
                                |> saveField "lon" (String.fromFloat place.lon)
                                |> saveField "lat" (String.fromFloat place.lat)
                                |> saveField "place" place.displayName
                            , mapFly (Just place.lon) (Just place.lat) )

                Err err ->
                    model |> toast (httpErrorMessage err "geocoderen")
                    
        ReverseGeocode result ->
            case result of
                Ok place ->
                    ( model
                        |> saveField "place" place.displayName
                    , Cmd.none )

                Err err ->
                    case err of
                        Http.BadPayload _ _ ->
                            ( model |> saveField "place" "", Cmd.none )
                        
                        _ ->
                            model |> toast (httpErrorMessage err "omgekeerd geocoderen")
        
        UrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChange url ->
            route { model | url = url }


---- VIEW ----


whiteTransparentBackground : Options.Property c m
whiteTransparentBackground =
    Options.css "background-color" "rgba(255, 255, 255, 0.77)"


textfieldValue : String -> Model -> Textfield.Property m
textfieldValue field model =
    let
        fieldModel = getField field model

        value_ = if fieldModel.focused
            then fieldModel.typed
            else fieldModel.saved
    in
    Textfield.value value_


ordinateTextField : String -> String -> Model -> Html Msg
ordinateTextField field label model =
    let
        index =
            "textfield-" ++ field
    in
    Textfield.view Mdc index model.mdc
        [ Textfield.label label
        , textfieldValue field model
        , Textfield.box
        , Textfield.pattern "-?\\d\\d?\\d?\\.?\\d*"
        , whiteTransparentBackground
        , Options.css "margin-right" ".5em"
        , Options.onInput (FieldInput field)
        , Options.onChange (FieldChange field)
        , Textfield.nativeControl
            [ Options.id (index ++ "-native")
            , Options.attribute <| size 10
            , Options.onFocus (FieldFocus field)
            , Options.on "keydown" <| D.map (FieldKey field) keyCode
            ]
        ]
        []


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Map Demo"
    , body =
        [ div []
            [ div [ id "place"
                , style "position" "absolute"
                , style "top" ".5em", style "left" ".5em"
                , style "width" "calc(100% - 1em)"
                ]
                [ Textfield.view Mdc "textfield-place" model.mdc
                    [ Textfield.label "Plek"
                    , textfieldValue "place" model
                    , Textfield.fullwidth
                    -- , Textfield.trailingIcon "cancel"
                    , whiteTransparentBackground
                    , Options.css "padding" "0 1em"
                    , Options.onInput (FieldInput "place")
                    , Options.onChange (FieldChange "place")
                    , Textfield.nativeControl
                        [ Options.id "textfield-place-native"
                        , Options.onFocus (FieldFocus "place")
                        , Options.on "keydown" <| D.map (FieldKey "place") keyCode
                        ]
                    ] []
                ]
            , div [ id "lonlat"
                , style "position" "absolute"
                , style "bottom" "0", style "left" ".5em"
                ]
                [ ordinateTextField "lon" "Lengtegraad" model
                , ordinateTextField "lat" "Breedtegraad" model
                ]
            , div [ id "map"
                , style "position" "absolute", style "top" "0"
                , style "width" "100%", style "height" "100%"
                , style "z-index" "-1"
                , style "background-image" "url('./logo.svg')"
                , style "background-position" "center center"
                , style "background-repeat" "no-repeat"
                , style "background-size" "30% 30%"
                ] []
            , Icon.view [ Options.id "icon-visor"
                , Options.css "position" "absolute"
                , Options.css "top" "50%", Options.css "left" "50%"
                , Options.css "transform" "translate(-50%, -50%)"
                , Options.css "user-select" "none"
                ] "gps_not_fixed"
            , Snackbar.view Mdc "my-snackbar" model.mdc [] []
            ]
        ]
    }


---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Material.subscriptions Mdc model
        , mapCenter MapCenter
        ]


---- PORTS ----


port mapCenter : (Coordinate -> msg) -> Sub msg


port map : E.Value -> Cmd msg


mapPan : (Maybe Float) -> (Maybe Float) -> Cmd msg
mapPan maybeLon maybeLat =
    "Pan" |> mapMove 300 maybeLon maybeLat


mapFly : (Maybe Float) -> (Maybe Float) -> Cmd msg
mapFly maybeLon maybeLat =
    "Fly" |> mapMove 2000 maybeLon maybeLat


mapMove : Int -> (Maybe Float) -> (Maybe Float) -> String -> Cmd msg
mapMove duration maybeLon maybeLat cmd =
    case maybeLon of
        Nothing ->
            Cmd.none
        
        Just lon ->
            case maybeLat of
                Nothing ->
                    Cmd.none
                
                Just lat ->
                    map (E.object
                        [ ("Cmd", E.string cmd)
                        , ("lon", E.float lon)
                        , ("lat", E.float lat)
                        , ("duration", E.int duration)
                        ]
                    )


port dom : E.Value -> Cmd msg


selectText : String -> Cmd msg
selectText id =
    dom (E.object
        [ ("Cmd", E.string "SelectText")
        , ("id", E.string id)
        ]
    )


---- PROGRAM ----


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChange
        , onUrlRequest = UrlRequest
        }
