port module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Task
import Array
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard.Key as Key
import Material
import Material.Button as Button
import Material.Options as Options
import Material.Textfield as Textfield
import Material.Snackbar as Snackbar
import Material.Icon as Icon
import Json.Encode as E
import Http
import Json.Decode as D
import Json.Decode.Extra as DE
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
        (D.field "lon" DE.parseFloat)
        (D.field "lat" DE.parseFloat)
        (D.field "display_name" D.string)


geocodeUrl : String -> String
geocodeUrl q = 
    Url.crossOrigin "https://nominatim.openstreetmap.org" ["search"]
        [ Url.string "q" q
        , Url.string "format" "json"
        ]


geocode : String -> Cmd Msg
geocode q =
    Http.send Geocode (Http.get (geocodeUrl q) (D.list placeDecoder)) 


reverseUrl : Float -> Float -> String
reverseUrl lon lat = 
    Url.crossOrigin "https://nominatim.openstreetmap.org" ["reverse"]
        [ Url.string "lon" (String.fromFloat lon)
        , Url.string "lat" (String.fromFloat lat)
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
                    Http.send ReverseGeocode (Http.get (reverseUrl lon lat) placeDecoder) 


---- MODEL ----


floatFormat : String -> String
floatFormat input =
    case (String.toFloat input) of
        Nothing ->
            ""
            
        Just float ->
            let
                parts =
                    Array.fromList (String.split "." input)
            in
            if Array.length parts == 2
            && String.length (Maybe.withDefault "" (Array.get 1 parts)) > 5 then
                let
                    absFloat =
                        abs float

                    fraction =
                        absFloat - toFloat (floor absFloat)

                    decimals =
                        String.fromInt (round (fraction * 100000))
                in
                (Maybe.withDefault "" (Array.get 0 parts)) ++ "." ++ decimals
            
            else
                input


type alias FieldModel =
    { old : String
    , new : String
    }


type alias Model =
    { mdc : Material.Model Msg
    , fields : Dict String FieldModel
    }


defaultModel : Model
defaultModel =
    { mdc = Material.defaultModel
    , fields = Dict.fromList
        [ ("lon", FieldModel "" "")
        , ("lat", FieldModel "" "")
        , ("place", FieldModel "" "")
        ]
    }


init : ( Model, Cmd Msg )
init =
    ( defaultModel, Cmd.batch
        [ Material.init Mdc
        , geocode "onze lieve vrouwetoren, amersfoort"
        ]
    )


---- UPDATE ----


type alias Coordinate =
    { lon : Float
    , lat : Float
    }


toast : Model -> String -> ( Model, Cmd Msg )
toast model message =
    let
        contents =
            Snackbar.toast Nothing message
        ( mdc, effects ) =
            Snackbar.add Mdc "my-snackbar" contents model.mdc
    in
        ( { model | mdc = mdc }, effects )


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


blur : String -> Cmd Msg
blur id =
    Task.attempt (\_ -> NoOp) (Dom.blur id)


latLon : Model -> Cmd Msg
latLon model =
    let
        lon =
            (getField "lon" model).new

        lat =
            (getField "lat" model).new
    in
    Cmd.batch
        [ mapFly (String.toFloat lon) (String.toFloat lat)
        , reverseGeocode (String.toFloat lon) (String.toFloat lat)
        ]


updateField : String -> Maybe String -> Maybe String -> Model -> Model
updateField field maybeOld maybeNew model =
    { model | fields =
        Dict.update field (\maybeValue ->
            case maybeValue of
                Nothing ->
                    Just (FieldModel "" "")
            
                Just value ->
                    Just { value
                        | old = Maybe.withDefault value.old maybeOld
                        , new = Maybe.withDefault value.new maybeNew
                    }
        ) model.fields
    }


getField : String -> Model -> FieldModel
getField field model =
    Maybe.withDefault (FieldModel "" "") (Dict.get field model.fields)


type Msg
    = Mdc (Material.Msg Msg)
    | NoOp
    | FieldInput String String
    | FieldKey String Int
    | FloatBlur String
    | TextBlur String
    | MapCenter Coordinate
    | Geocode (Result Http.Error (List PlaceModel))
    | ReverseGeocode (Result Http.Error PlaceModel)
    | SelectText String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdc msg_ ->
            Material.update Mdc msg_ model
        
        NoOp ->
            ( model, Cmd.none )
        
        FieldInput field input ->
            ( updateField field Nothing (Just input) model, Cmd.none )
        
        FieldKey field code ->
            let
                id =
                    "textfield-" ++ field ++ "-native"
            in
            case (Key.fromCode code) of
                Key.Enter ->
                    ( model, blur id )
                
                Key.Escape ->
                    let
                        old =
                            (getField field model).old
                    in
                    ( updateField field Nothing (Just old) model, blur id )
                
                _ ->
                    ( model, Cmd.none )

        FloatBlur field ->
            let
                fieldModel =
                    getField field model

                text =
                    floatFormat fieldModel.new
            in
            if fieldModel.old == text then
                ( updateField field Nothing (Just text) model, Cmd.none )
            
            else
                ( updateField field (Just text) (Just text) model, latLon model )

        TextBlur field ->
            let
                fieldModel =
                    getField field model
            in
            if fieldModel.new == fieldModel.old then
                ( model, Cmd.none )
            
            else
                ( updateField field (Just fieldModel.new) Nothing model, geocode fieldModel.new )

        MapCenter coordinate ->
            let
                lon =
                    floatFormat (String.fromFloat coordinate.lon)

                lat =
                    floatFormat (String.fromFloat coordinate.lat)
            in
            if lon == (getField "lon" model).new
            && lat == (getField "lat" model).new then
                ( model, Cmd.none )
            
            else
                ( updateField "lat" (Just lat) (Just lat) (
                    updateField "lon" (Just lon) (Just lon) model
                ), reverseGeocode (Just coordinate.lon) (Just coordinate.lat) )
                                
        Geocode result ->
            case result of
                Ok places ->
                    case (List.head places) of
                        Nothing ->
                            ( model, Cmd.none )
                            
                        Just place ->
                            let
                                lon =
                                    floatFormat (String.fromFloat place.lon)

                                lat =
                                    floatFormat (String.fromFloat place.lat)
                            in
                            ( updateField "lat" (Just lat) (Just lat) (
                                updateField "lon" (Just lon) (Just lon) (
                                    updateField "place" (Just place.displayName) (Just place.displayName) model
                                )
                            ), mapFly (Just place.lon) (Just place.lat) )

                Err err ->
                    toast model (httpErrorMessage err "geocoderen")
                    
        
        ReverseGeocode result ->
            case result of
                Ok place ->
                    ( updateField "place" (Just place.displayName) (Just place.displayName) model, Cmd.none )

                Err err ->
                    toast model (httpErrorMessage err "omgekeerd geocoderen")

        SelectText field  ->
            ( model, selectText ("textfield-" ++ field ++ "-native") )


---- VIEW ----


ordinateTextField :  Model -> String -> String -> Html Msg
ordinateTextField model field label =
    let
        index =
            "textfield-" ++ field
    in
    Textfield.view Mdc index model.mdc
        [ Textfield.label label
        , Textfield.value (getField field model).new
        , Textfield.box
        , Textfield.pattern "-?\\d\\d?\\d?\\.?\\d*"
        , Options.css "background-color" "rgba(255, 255, 255, 0.77)"
        , Options.css "margin-left" ".5em"
        , Options.onInput (FieldInput field)
        , Textfield.nativeControl
            [ Options.id (index ++ "-native")
            , Options.onFocus (SelectText field)
            , Options.onBlur (FloatBlur field)
            , Options.on "keydown" (D.map (FieldKey field) keyCode)
            ]
        ]
        []


view : Model -> Html Msg
view model =
    div []
        [ div [ id "place"
            , style "position" "absolute"
            , style "top" ".5em", style "left" "3em"
            , style "width" "calc(100% - 6em)"
            ]
            [ Textfield.view Mdc "textfield-place" model.mdc
                [ Textfield.label "Plek"
                , Textfield.value (getField "place" model).new
                , Textfield.fullwidth
                -- , Textfield.trailingIcon "cancel"
                , Options.css "background-color" "rgba(255, 255, 255, 0.77)"
                , Options.css "padding" "0 1em"
                , Options.onInput (FieldInput "place")
                , Textfield.nativeControl
                    [ Options.id "textfield-place-native"
                    , Options.onFocus (SelectText "place")
                    , Options.on "keydown" (D.map (FieldKey "place") keyCode)
                    , Options.onBlur (TextBlur "place")
                    ]
                ] []
            ]
        , div [ id "lonlat"
            , style "position" "absolute", style "bottom" "0"
            ]
            [ ordinateTextField model "lon" "Lengtegraad"
            , ordinateTextField model "lat" "Breedtegraad"
            ]
        , div [ id "map"
            , style "position" "absolute", style "top" "0"
            , style "width" "100%", style "height" "100%"
            , style "z-index" "-1"
            ] []
        , Icon.view [ Options.id "icon-visor"
            , Options.css "position" "absolute"
            , Options.css "top" "50%", Options.css "left" "50%"
            , Options.css "transform" "translate(-50%, -50%)"
            ] "gps_not_fixed"
        , Snackbar.view Mdc "my-snackbar" model.mdc [] []
        ]


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


mapFly : (Maybe Float) -> (Maybe Float) -> Cmd msg
mapFly maybeLon maybeLat =
    case maybeLon of
        Nothing ->
            Cmd.none
        
        Just lon ->
            case maybeLat of
                Nothing ->
                    Cmd.none
                
                Just lat ->
                    map (E.object
                        [ ("Cmd", E.string "Fly")
                        , ("lon", E.float lon)
                        , ("lat", E.float lat)
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


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
