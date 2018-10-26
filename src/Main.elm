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
    }


placeDecoder : D.Decoder PlaceModel
placeDecoder =
    D.map3 PlaceModel
        (D.field "lon" D.parseFloat)
        (D.field "lat" D.parseFloat)
        (D.field "display_name" D.string)


geocode : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
geocode q ( model, cmd ) =
    let
        url =
            Url.crossOrigin "https://nominatim.openstreetmap.org" ["search"]
                [ Url.string "q" q
                , Url.string "format" "json"
                ]
    in
    ( model, Cmd.batch
        [ cmd
        , Http.send Geocode <| Http.get url (D.list placeDecoder)
        ] )


reverseGeocode : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
reverseGeocode ( model, cmd ) =
    let
        url =
            Url.crossOrigin "https://nominatim.openstreetmap.org" ["reverse"]
                [ Url.string "lon" <| fieldValue "lon" model
                , Url.string "lat" <| fieldValue "lat" model
                , Url.string "format" "json"
                ]
    in
    ( model, Cmd.batch
        [ cmd
        , Http.send ReverseGeocode <| Http.get url placeDecoder
        ] )


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


type alias Model =
    { mdc : Material.Model Msg
    , url : Url.Url
    , key : Nav.Key
    , fields : Dict String FieldModel
    , zoom : Float
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
    , zoom = 15
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


getZoom : Maybe Float -> Model -> Float
getZoom maybeZoom model =
    case maybeZoom of
        Nothing ->
            model.zoom
    
        Just zoom ->
            if zoom < 0 then
                0 - zoom
            
            else
                zoom


setZoom : Maybe Float -> Model -> Model
setZoom maybeZoom model =
    { model | zoom = model |> getZoom maybeZoom }


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Home    (Parser.top)
        , Parser.map Search  (Parser.s "search" <?> Query.string "query" <?> queryFloat "zoom")
        , Parser.map Reverse (Parser.s "reverse" <?> queryFloat "lon" <?> queryFloat "lat" <?> queryFloat "zoom")
        ]


type Route
    = Home
    | Search (Maybe String) (Maybe Float)
    | Reverse (Maybe Float) (Maybe Float) (Maybe Float)
            

route : Model -> ( Model, Cmd Msg )
route model =
    case Parser.parse routeParser model.url of
        Nothing ->
            ( model, Nav.pushUrl model.key "/" )

        Just route_ ->
            case route_ of
                Home ->
                    navReverse_ False Nothing
                        "-76.8163" "42.08842"
                        ( model, Cmd.none )
            
                Search maybeQuery maybeZoom ->
                    case maybeQuery of
                        Nothing ->
                            model |> toast "Geen zoekopdracht gevonden"
                    
                        Just query ->
                            geocode query
                                ( model
                                    |> setZoom maybeZoom
                                , Cmd.none )
            
                Reverse maybeLon maybeLat maybeZoom ->
                    case maybeLon of
                        Nothing ->
                            model |> toast "Geen geldige lengtegraad"
                    
                        Just lon ->
                            case maybeLat of
                                Nothing ->
                                    model |> toast "Geen geldige breedtegraad"
                            
                                Just lat ->
                                    if lon > 180 || lon < -180 then
                                        model |> toast "Geen geldige lengtegraad"

                                    else if lat > 90 || lat < -90 then
                                        model |> toast "Geen geldige breedtegraad"
                                    
                                    else
                                        reverseGeocode
                                            ( model
                                                |> setZoom maybeZoom
                                                |> saveField "lon"
                                                    (String.fromFloat lon)
                                                |> saveField "lat"
                                                    (String.fromFloat lat)
                                            , Cmd.none )
                                            |> mapMove


navSearch : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
navSearch ( model, cmd ) =
    ( model, Cmd.batch
        [ cmd
        , Nav.pushUrl model.key <| Url.relative [ "search" ]
            [ Url.string "query" <| fieldValue "place" model
            , Url.string "zoom" <| String.fromFloat model.zoom
            ]
        ] )


navReverse : Maybe Float -> String -> String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
navReverse maybeZoom lon lat ( model, cmd ) =
    navReverse_ True maybeZoom lon lat ( model, cmd )


navReverse_ :  Bool -> Maybe Float -> String -> String -> ( Model, Cmd Msg ) ->( Model, Cmd Msg )
navReverse_ push maybeZoom lon lat ( model, cmd ) =
    let
        lonValue =
            (getField "lon" model).format lon

        latValue =
            (getField "lat" model).format lat

        ( zoomEqual, zoomString ) = 
            case maybeZoom of
                Nothing ->
                    ( True, String.fromFloat model.zoom )
            
                Just zoom ->
                    ( zoom == model.zoom, String.fromFloat zoom )
    in
    if lonValue == fieldValue "lon" model
    && latValue == fieldValue "lat" model
    && zoomEqual then
        ( model, cmd )

    else
        let
            fn =
                case push of
                    True ->
                        Nav.pushUrl
                
                    False ->
                        Nav.replaceUrl
        in
        ( model, Cmd.batch
            [ cmd
            , fn model.key <| Url.relative [ "reverse" ]
                [ Url.string "lon" lonValue
                , Url.string "lat" latValue
                , Url.string "zoom" zoomString
                ]
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
    | FieldKey String Int
    | FieldInput String String
    | FieldChange String String
    | MapMoved MapView
    | Geocode (Result Http.Error (List PlaceModel))
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
                case field of
                    "lon" ->
                        ( model, Cmd.none )
                            |> navReverse Nothing
                                input (fieldValue "lat" model)
                        
                    "lat" ->
                        ( model, Cmd.none )
                            |> navReverse Nothing
                                (fieldValue "lon" model) input
                        
                    "place" ->
                        ( model |> saveField field input, Cmd.none )
                            |> navSearch
                    
                    _ ->
                        ( model, Cmd.none )

        MapMoved mapView ->
            ( model, Cmd.none )
                |> navReverse (Just mapView.zoom)
                    (String.fromFloat mapView.center.lon)
                    (String.fromFloat mapView.center.lat)
                                
        Geocode result ->
            case result of
                Ok places ->
                    case (List.head places) of
                        Nothing ->
                            model |> toast "Niets gevonden"
                            
                        Just place ->
                            mapMove ( model
                                |> saveField "lon" (String.fromFloat place.lon)
                                |> saveField "lat" (String.fromFloat place.lat)
                                |> saveField "place" place.displayName
                            , Cmd.none )

                Err err ->
                    model |> toast (httpErrorMessage err "geocoderen")
                    
        ReverseGeocode result ->
            case result of
                Ok place ->
                    ( model
                        |> saveField "place" place.displayName
                    , Cmd.none )

                Err err ->
                    let
                       newmodel =
                           model |> saveField "place" "" 
                    in
                    case err of
                        Http.BadPayload _ _ ->
                            ( newmodel, Cmd.none )
                        
                        _ ->
                            newmodel |> toast (httpErrorMessage err "omgekeerd geocoderen")


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


port map : E.Value -> Cmd msg


mapMove : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
mapMove ( model, cmd ) =
    let
        lon =
           String.toFloat (fieldValue "lon" model) |> Maybe.withDefault 0 

        lat =
           String.toFloat (fieldValue "lat" model) |> Maybe.withDefault 0 
    in
    ( model, Cmd.batch
        [ cmd
        , map <| E.object
            [ ("Cmd", E.string "Fly")
            , ("lon", E.float lon)
            , ("lat", E.float lat)
            , ("zoom", E.float model.zoom)
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
