port module Main exposing (..)

import Material
import Material.Button as Button
import Material.Options as Options
import Material.Textfield as Textfield
import Material.Snackbar as Snackbar
import Material.Icon as Icon
import Material.Menu as Menu
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Http
import Url
import Url.Builder as Url
import Url.Parser as Parser exposing (Parser, (</>), (<?>))
import Url.Parser.Query as Query
import Keyboard.Key as Key
import Json.Encode as E
import Json.Decode as D
import Json.Decode.Extra as D
import Task
import List.Extra as List
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import GeoJson


---- HTTP ----


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


type alias PlaceModel =
    { lon : Float
    , lat : Float
    , displayName : String
    , geoJson : GeoJson.GeoJson
    }


placeDecoder : D.Decoder PlaceModel
placeDecoder =
    D.map4 PlaceModel
        (D.field "lon" D.parseFloat)
        (D.field "lat" D.parseFloat)
        (D.field "display_name" D.string)
        (D.field "geojson" GeoJson.decoder)


geocode : String -> Model -> ( Model, Cmd Msg )
geocode q model =
    let
        url =
            Url.crossOrigin "https://nominatim.openstreetmap.org" ["search"]
                [ Url.string "q" q
                , Url.string "format" "json"
                , Url.int "polygon_geojson" 1
                ]
    in
    ( model, Http.send Geocode <| Http.get url (D.list placeDecoder) )


reverseGeocode : Model -> ( Model, Cmd Msg )
reverseGeocode model =
    let
        url =
            Url.crossOrigin "https://nominatim.openstreetmap.org" ["reverse"]
                [ Url.string "lon" <| fieldValue "lon" model
                , Url.string "lat" <| fieldValue "lat" model
                , Url.int "zoom" <| round model.zoom
                , Url.string "format" "json"
                , Url.int "polygon_geojson" 1
                ]
    in
    ( model, Http.send ReverseGeocode <| Http.get url placeDecoder )


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
    FieldModel "" "" False True (\v -> v)


floatFormat : String -> String
floatFormat input =
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
            if List.length parts == 2 && String.length last > 5 then
                let
                    fraction =
                        String.toFloat ("0.1" ++ last) |> Maybe.withDefault 0

                    decimals =
                        round (fraction * 1000000)
                    
                    front =
                        if decimals == 200000 then
                            let
                                num =
                                    String.toInt first |> Maybe.withDefault 0
                                
                                next =
                                    abs num + 1
                                
                                val =
                                    if num < 0 then
                                        0 - next
                                    
                                    else
                                        next
                            in
                            String.fromInt val
                        
                        else
                            first
                in
                front ++ "." ++ (String.dropLeft 1 <| String.fromInt decimals)

            else
                input


lonLatFieldModel : String -> FieldModel
lonLatFieldModel defaultValue =
    FieldModel (floatFormat defaultValue) (floatFormat defaultValue) False False floatFormat


validateLonLat : ( Maybe Float, Maybe Float ) -> Maybe ( String, String )
validateLonLat ( maybeLon, maybeLat ) =
    case maybeLon of
        Nothing ->
            Nothing

        Just lon ->
            if lon < -180 || lon > 180 then
                Nothing
            
            else
                case maybeLat of
                    Nothing ->
                        Nothing

                    Just lat ->
                        if lat < -90 || lat > 90 then
                            Nothing
                        
                        else
                            Just <| mapBothSame
                                (\v -> floatFormat <| String.fromFloat v)
                                ( lon, lat )


validatePlace : Maybe String -> Maybe String
validatePlace maybeString =
    case maybeString of
        Nothing ->
            Nothing
    
        Just string ->
            if string == "" then
                Nothing

            else
                Just string


type alias Model =
    { mdc : Material.Model Msg
    , url : Url.Url
    , key : Nav.Key
    , fields : Dict String FieldModel
    , zoom : Float
    , geoJson : Maybe GeoJson.GeoJson
    , moving : Bool
    , places : List PlaceModel
    }


defaultModel : Url.Url -> Nav.Key -> Model
defaultModel url key =
    { mdc = Material.defaultModel
    , url = url
    , key = key
    , fields = Dict.fromList
        [ ("lon", lonLatFieldModel "-76.81645")
        , ("lat", lonLatFieldModel "42.08871")
        , ("place", defaultFieldModel)
        ]
    , zoom = 17
    , geoJson = Nothing
    , moving = False
    , places = []
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


setZoom : Maybe Float -> Model -> Model
setZoom maybeZoom model =
    case maybeZoom of
        Nothing ->
            model
    
        Just zoom ->
            { model | zoom = zoom }


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Home    (Parser.top)
        , Parser.map Search  (Parser.s "search" <?> Query.string "query")
        , Parser.map Reverse (Parser.s "reverse" <?> queryFloat "lon" <?> queryFloat "lat" <?> queryFloat "zoom")
        ]


type Route
    = Home
    | Search (Maybe String)
    | Reverse (Maybe Float) (Maybe Float) (Maybe Float)
            

route : Model -> ( Model, Cmd Msg )
route model =
    case Parser.parse routeParser model.url of
        Nothing ->
            ( model, Nav.replaceUrl model.key "/" )

        Just route_ ->
            case route_ of
                Home ->
                    reverseGeocode model
            
                Search maybeQuery ->
                    case validatePlace maybeQuery of
                        Nothing ->
                            toast "Geen geldige zoekopdracht" model
                    
                        Just query ->
                            geocode query model
            
                Reverse maybeLon maybeLat maybeZoom ->
                    case validateLonLat ( maybeLon, maybeLat ) of
                        Nothing ->
                            toast "Geen geldige coördinaten" model
                    
                        Just ( lonString, latString ) ->
                            reverseGeocode (model
                                |> setZoom maybeZoom
                                |> saveField "lon" lonString
                                |> saveField "lat" latString
                            )


navSearch : Model -> ( Model, Cmd Msg )
navSearch model =
    ( model, Nav.pushUrl model.key <| Url.relative [ "search" ]
        [ Url.string "query" <| fieldValue "place" model
        ] )


navReverse : Maybe Float -> ( String, String ) -> Model -> ( Model, Cmd Msg )
navReverse maybeZoom ( lon, lat ) model =
    ( model, Nav.pushUrl model.key <| Url.relative [ "reverse" ]
        [ Url.string "lon" <| floatFormat lon
        , Url.string "lat" <| floatFormat lat
        , Url.string "zoom" <| String.fromFloat (Maybe.withDefault model.zoom maybeZoom)
        ] )


---- UPDATE ----


type alias Coordinate =
    { lon : Float
    , lat : Float
    }


type alias MapView =
    { center : Coordinate
    , zoom : Float
    }


type Msg
    = Mdc (Material.Msg Msg)
    | NoOp
    | UrlRequest Browser.UrlRequest
    | UrlChange Url.Url
    | FieldFocus String
    | FieldBlur String
    | FieldKey String Int
    | FieldInput String String
    | FieldChange String String
    | MapMoved MapView
    | Geocode (Result Http.Error (List PlaceModel))
    | Select PlaceModel
    | ReverseGeocode (Result Http.Error PlaceModel)
    

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdc msg_ ->
            Material.update Mdc msg_ model
        
        NoOp ->
            ( model, Cmd.none )
        
        UrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChange url ->
            route { model | url = url }

        FieldFocus field ->
            ( model |> focusField field True
            , if (getField field model).select
                then selectText <| "textfield-" ++ field ++ "-native"
                else Cmd.none )

        FieldBlur field ->
            ( model |> focusField field False
            , Cmd.none )
        
        FieldKey field code ->
            case (Key.fromCode code) of
                Key.Enter ->
                    ( model, blur field )
                
                Key.Escape ->
                    ( model |> untypeField field, blur field )
                
                _ ->
                    ( model, Cmd.none )

        FieldInput field input ->
            ( model |> typeField field input, Cmd.none )

        FieldChange field input ->
            case field of
                "place" ->
                    case validatePlace <| Just input of
                        Nothing ->
                            ( untypeField field model, Cmd.none )
                            
                        Just place ->
                            navSearch <| saveField field place model
                
                "lon" ->
                    lonLatChange field model
                        ( input, fieldValue "lat" model )
                    
                "lat" ->
                    lonLatChange field model
                        ( fieldValue "lon" model, input )
                    
                _ ->
                    ( model, Cmd.none )

        MapMoved mapView ->
            case model.moving of
                True ->
                    ( { model | moving = False, zoom = mapView.zoom }
                        |> saveField "lon" (String.fromFloat mapView.center.lon)
                        |> saveField "lat" (String.fromFloat mapView.center.lat)
                    , Cmd.none )
            
                False ->
                    model |> navReverse
                        (Just mapView.zoom)
                        (mapBothSame String.fromFloat
                            ( mapView.center.lon, mapView.center.lat ))
                                
        Geocode result ->
            case result of
                Ok places ->
                    case List.head places of
                        Nothing ->
                            unselectPlace "Niets gevonden" model
                            
                        Just place ->
                            case List.length places of
                                1 ->
                                    selectPlace place model
                            
                                _ ->
                                    ( { model | places = places }, Cmd.none )

                Err err ->
                    unselectPlace (httpErrorMessage err "geocoderen") model
        
        Select place ->
            selectPlace place model
                    
        ReverseGeocode result ->
            let
                newmodel =
                    { model | places = [] }
            in
            case result of
                Ok place ->
                    mapFly ( { newmodel | geoJson = Just place.geoJson }
                        |> saveField "place" place.displayName
                    , Cmd.none )

                Err err ->
                    let
                       errmodel =
                            { newmodel | geoJson = Nothing }
                                |> saveField "place" "" 
                    in
                    mapFly <| case err of
                        Http.BadPayload _ _ ->
                            ( errmodel, Cmd.none )
                        
                        _ ->
                            toast (httpErrorMessage err "omgekeerd geocoderen") errmodel


lonLatChange : String -> Model -> ( String, String ) -> ( Model, Cmd Msg )
lonLatChange field model ( lon, lat ) =
    case validateLonLat <| mapBothSame String.toFloat ( lon, lat ) of
        Nothing ->
            ( untypeField field model, Cmd.none )
    
        Just ( lonString, latString ) ->
            navReverse Nothing ( lonString, latString ) model


selectPlace : PlaceModel -> Model -> ( Model, Cmd Msg )
selectPlace place model =
    mapFit ( { model | geoJson = Just place.geoJson }
        |> saveField "lon" (String.fromFloat place.lon)
        |> saveField "lat" (String.fromFloat place.lat)
        |> saveField "place" place.displayName
    , Cmd.none )


unselectPlace : String -> Model -> ( Model, Cmd Msg )
unselectPlace message model =
    mapFit <| toast message { model
        | geoJson = Nothing
        , places = []
    }


blur : String -> Cmd Msg
blur field =
    Task.attempt (\_ -> NoOp) <| Dom.blur ("textfield-" ++ field ++ "-native")


toast : String -> Model -> ( Model, Cmd Msg )
toast message model =
    let
        contents =
            Snackbar.toast Nothing message
        ( mdc, effects ) =
            Snackbar.add Mdc "my-snackbar" contents model.mdc
    in
    ( { model | mdc = mdc }, effects )


---- VIEW ----


whiteTransparentBackground : Options.Property c m
whiteTransparentBackground =
    Options.css "background-color" "rgba(255, 255, 255, 0.77)"


textfieldValue : String -> Model -> Textfield.Property m
textfieldValue field model =
    let
        fieldModel =
            getField field model

        value_ =
            if fieldModel.focused
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
            , Options.onBlur (FieldBlur field)
            , Options.on "keydown" <| D.map (FieldKey field) keyCode
            ]
        ]
        []


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Street"
    , body =
        [ div []
            [ div [ id "place"
                , style "position" "absolute"
                , style "top" ".5em", style "left" ".5em"
                , style "width" "calc(100% - 1em)"
                , class "mdc-menu-anchor"
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
                        , Options.onBlur (FieldBlur "place")
                        , Options.on "keydown" <| D.map (FieldKey "place") keyCode
                        ]
                    , Menu.attach Mdc "places-menu"
                    ] []
                , case List.length model.places of
                    0 ->
                        text ""
                
                    _ ->
                        Menu.view Mdc "places-menu" model.mdc
                            [ Menu.anchorCorner Menu.topLeftCorner
                            , Menu.anchorMargin {top = 56, left = 0, bottom = 0, right = 0}
                            ]
                            ( Menu.ul [] <|
                                List.map (\place ->
                                    Menu.li
                                        [ Menu.onSelect (Select place)
                                        ]
                                        [ text place.displayName
                                        ]
                                ) model.places
                            )
                ]
            , div [ id "lonlat"
                , style "position" "absolute"
                , style "bottom" "0", style "left" ".5em"
                , style "max-width" "calc(100% - 1em - .5em - 1.375em - 6px)"
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
        , mapMoved MapMoved
        ]


---- PORTS ----


port mapMoved : (MapView -> msg) -> Sub msg


port mapPort : E.Value -> Cmd msg


lonLatValue : String -> Model -> E.Value
lonLatValue field model =
    case String.toFloat (fieldValue field model) of
        Nothing ->
            E.float 0
    
        Just float ->
            E.float float


geoJsonValue : Model -> E.Value
geoJsonValue model =
    case model.geoJson of
        Nothing ->
            E.null
    
        Just geoJson ->
            GeoJson.encode geoJson


mapFly : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
mapFly ( model, cmd ) =
    ( { model | moving = True }, Cmd.batch
        [ cmd
        , mapPort <| E.object
            [ ("Cmd", E.string "Fly")
            , ("lon", lonLatValue "lon" model)
            , ("lat", lonLatValue "lat" model)
            , ("zoom", E.float model.zoom)
            , ("geoJson", geoJsonValue model)
            ]
        ] )


mapFit : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
mapFit ( model, cmd ) =
    ( { model | moving = True }, Cmd.batch
        [ cmd
        , mapPort <| E.object
            [ ("Cmd", E.string "Fit")
            , ("geoJson", geoJsonValue model)
            ]
        ] )


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


mapBothSame : (a -> x) -> ( a, a ) -> ( x, x )
mapBothSame fn tuple =
    Tuple.mapBoth fn fn tuple
