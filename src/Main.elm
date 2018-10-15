port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Material
import Material.Button as Button
import Material.Options as Options
import Material.Textfield as Textfield
import Material.Snackbar as Snackbar
import Json.Encode as E
import Http
import Json.Decode as D
import Json.Decode.Extra as DE
import Url.Builder as Url


---- HTTP ----


type alias Place =
    { lon : Float
    , lat : Float
    , displayName : String
    }


placeDecoder : D.Decoder Place
placeDecoder =
    D.map3 Place
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
    }


defaultModel : Model
defaultModel =
    { mdc = Material.defaultModel
    , lon = FloatField "" Nothing
    , lat = FloatField "" Nothing
    , place = ""
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


type Msg
    = Mdc (Material.Msg Msg)
    | Lon String
    | Lat String
    | MapFly
    | MapCenter Coordinate
    | GeocodeQuery String
    | SendGeocode
    | Geocode (Result Http.Error (List Place))


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdc msg_ ->
            Material.update Mdc msg_ model

        Lon lon ->
            ( { model | lon = FloatField lon (String.toFloat lon) }, Cmd.none )

        Lat lat ->
            ( { model | lat = FloatField lat (String.toFloat lat) }, Cmd.none )

        MapFly ->
            ( model, mapFly model.lon.value model.lat.value )

        MapCenter coordinate ->
            ( { model
                | lon = FloatField (String.fromFloat (decimals coordinate.lon 5)) (Just coordinate.lon)
                , lat = FloatField (String.fromFloat (decimals coordinate.lat 5)) (Just coordinate.lat)
            }, Cmd.none )

        GeocodeQuery query ->
            ( { model | place = query }, Cmd.none )

        SendGeocode ->
            ( model, geocode model.place )
        
        Geocode result ->
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


ordinateTextField : String -> String -> String -> Model -> (String -> Msg) -> Html Msg
ordinateTextField index label value model msg =
    Textfield.view Mdc index model.mdc
        [ Textfield.label label
        , Textfield.value value
        , Textfield.pattern "\\d+\\.?\\d*"
        , Options.css "background-color" "rgba(255, 255, 255, 0.8)"
        , Options.onInput msg
        , Textfield.nativeControl
            [ Options.onBlur MapFly
            ]
        ]
        []


view : Model -> Html Msg
view model =
    div []
        [ Textfield.view Mdc "textfield-q" model.mdc
            [ Textfield.label "Plek"
            , Textfield.value model.place
            , Options.css "background-color" "rgba(255, 255, 255, 0.8)"
            , Options.onInput GeocodeQuery
            , Textfield.nativeControl
                [ Options.onBlur SendGeocode
                ]
            ]
            []
        , ordinateTextField "textfield-lon" "Lengtegraad" model.lon.input model Lon
        , ordinateTextField "textfield-lat" "Breedtegraad" model.lat.input model Lat
        , div
            [ id "map"
            , style "position" "absolute" , style "top" "0" , style "left" "0"
            , style "width" "100%" , style "height" "100%"
            , style "z-index" "-1"
            ] []
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


mapInit : Cmd msg
mapInit =
    map (E.object [ ("Cmd", E.string "Init") ])


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


---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
