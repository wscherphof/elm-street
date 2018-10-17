port module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Task
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
    Http.send Places (Http.get (geocodeUrl q) (D.list placeDecoder)) 


---- MODEL ----


type alias FloatField =
    { input : String
    , value : Maybe Float
    }


type alias Model =
    { mdc : Material.Model Msg
    , lon : FloatField
    , lat : FloatField
    , place : String
    , dirty : Bool
    }


defaultModel : Model
defaultModel =
    { mdc = Material.defaultModel
    , lon = FloatField "" Nothing
    , lat = FloatField "" Nothing
    , place = ""
    , dirty = False
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


decimals : Float -> Int -> Float
decimals given number =
    let
        times =
            toFloat (10 ^ number)
    in
    toFloat (round (given * times)) / times


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


blur : Int -> String -> Cmd Msg
blur keycode id =
    let
        attempt =
            case (Key.fromCode keycode) of
                Key.Enter ->
                    True
                
                Key.Escape ->
                    True
                
                _ ->
                    False
    in
    if attempt then
        Task.attempt (\_ -> NoOp) (Dom.blur id)

    else
        Cmd.none


type Msg
    = Mdc (Material.Msg Msg)
    | NoOp
    | Lon String
    | LonKey Int
    | Lat String
    | LatKey Int
    | MapFly
    | MapCenter Coordinate
    | Place String
    | PlaceKey Int
    | PlaceSelect
    | PlaceBlur
    | Places (Result Http.Error (List PlaceModel))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdc msg_ ->
            Material.update Mdc msg_ model
        
        NoOp ->
            ( model, Cmd.none )

        Lon lon ->
            ( { model | lon = FloatField lon (String.toFloat lon) }, Cmd.none )
        
        LonKey code ->
            ( model, (blur code "textfield-lon-native" ) )

        Lat lat ->
            ( { model | lat = FloatField lat (String.toFloat lat) }, Cmd.none )
        
        LatKey code ->
            ( model, (blur code "textfield-lat-native" ) )

        MapFly ->
            ( { model | dirty = True }, mapFly model.lon.value model.lat.value )

        MapCenter coordinate ->
            ( { model
                | dirty = True
                , lon = FloatField (String.fromFloat (decimals coordinate.lon 5)) (Just coordinate.lon)
                , lat = FloatField (String.fromFloat (decimals coordinate.lat 5)) (Just coordinate.lat)
            }, Cmd.none )

        Place query ->
            ( { model | dirty = True, place = query }, Cmd.none )
        
        PlaceKey code ->
            ( model, (blur code "textfield-place-native" ) )

        PlaceSelect ->
            ( model, selectText "textfield-place-native" )

        PlaceBlur ->
            case model.dirty of
                True ->
                    ( { model | dirty = False }, geocode model.place )
            
                False ->
                    ( model, Cmd.none )
                    
        
        Places result ->
            case result of
                Ok places ->
                    case (List.head places) of
                        Nothing ->
                            ( model, Cmd.none )
                            
                        Just place ->
                            ( { model
                                | lon = FloatField (String.fromFloat (decimals place.lon 5)) (Just place.lon)
                                , lat = FloatField (String.fromFloat (decimals place.lat 5)) (Just place.lat)
                                , place = place.displayName
                            }, mapFly (Just place.lon) (Just place.lat) )

                Err err ->
                    toast model (httpErrorMessage err "geocoderen")


---- VIEW ----


ordinateTextField :  Model -> String -> String -> String -> (String -> Msg) -> (Int -> Msg) -> Html Msg
ordinateTextField model index label value inputMsg keyMsg =
    Textfield.view Mdc index model.mdc
        [ Textfield.label label
        , Textfield.value value
        , Textfield.box
        , Textfield.pattern "\\d+\\.?\\d*"
        , Options.css "background-color" "rgba(255, 255, 255, 0.77)"
        , Options.css "margin-left" ".5em"
        , Options.onInput inputMsg
        , Textfield.nativeControl
            [ Options.id (index ++ "-native")
            , Options.onBlur MapFly
            , Options.on "keydown" (D.map keyMsg keyCode)
            ]
        ]
        []


view : Model -> Html Msg
view model =
    div []
        [ div [ id "place"
            , style "position" "absolute"
            , style "top" ".5em", style "left" "3em"
            , style "width" "calc(100% - 4em)"
            ]
            [ Textfield.view Mdc "textfield-place" model.mdc
                [ Textfield.label "Plek"
                , Textfield.value model.place
                , Textfield.fullwidth
                -- , Textfield.trailingIcon "cancel"
                , Options.css "background-color" "rgba(255, 255, 255, 0.77)"
                , Options.css "padding" "0 1em"
                , Options.onInput Place
                , Textfield.nativeControl
                    [ Options.id "textfield-place-native"
                    , Options.onFocus PlaceSelect
                    , Options.on "keydown" (D.map PlaceKey keyCode)
                    , Options.onBlur PlaceBlur
                    ]
                ] []
            ]
        , div [ id "lonlat"
            , style "position" "absolute", style "bottom" "0"
            ]
            [ ordinateTextField model "textfield-lon" "Lengtegraad" model.lon.input Lon LonKey
            , ordinateTextField model "textfield-lat" "Breedtegraad" model.lat.input Lat LatKey
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
