module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Attrs
import Html.Events as Events
import Http
import Task
import Date
import Json.Decode exposing (..)


type alias Model =
  { product: Maybe Product
  }


type Msg
  = FetchA
  | FetchB
  | DateString
  | DateTimestamp
  | Fetched (Result Http.Error Product)


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

init : ( Model, Cmd Msg )
init =
  ( initModel, Cmd.none )


initModel : Model
initModel =
  { product = Nothing
  }


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    FetchA ->
      ( { model | product = Nothing }, fetchA )

    FetchB ->
      ( { model | product = Nothing }, fetchB )

    Fetched (Ok product) ->
      ( { model | product = Just (Debug.log "product" product) }, Cmd.none )

    DateString ->
      let
        _ = Debug.log "Decode string date" ( Json.Decode.decodeString decodeDateFromString "\"2005-08-09T18:31:42\"" )
      in
        ( model, Cmd.none )

    DateTimestamp ->
      let
        _ = Debug.log "Decode integer timestamp" ( Json.Decode.decodeString decodeDateFromTimestamp "1537940952" )
        _ = Debug.log "Decode float integer timestamp" ( Json.Decode.decodeString decodeDateFromTimestamp "1537940952.00" )
      in
        ( model, Cmd.none )

    Fetched (Err reason) ->
      let
        _ = Debug.log "reason" reason
      in
        ( { model | product = Nothing }, Cmd.none )


view : Model -> Html Msg
view model =
  div []
    [ button [ Events.onClick FetchA ] [ text "FetchA" ]
    , button [ Events.onClick FetchB ] [ text "FetchB" ]
    , button [ Events.onClick DateString ] [ text "Decode string date" ]
    , button [ Events.onClick DateTimestamp ] [ text "Decode timestamp" ]
    ]


fetchA : Cmd Msg
fetchA =
  Http.get
    "/decoder_encoder/fetchA.json"
    decodeProduct
    |> Http.toTask
    |> Task.attempt Fetched


fetchB : Cmd Msg
fetchB =
  Http.get
    "/decoder_encoder/fetchB.json"
    decodeProduct2
    |> Http.toTask
    |> Task.attempt Fetched


type alias Product =
  { id: Int
  , name: String
  , price: Int
  , attributes: Attributes
  }

type alias Attributes = List Attribute

type alias Attribute =
  { id: Int
  , name: String
  , unit: String
  , value: AttributeValue
  }

type AttributeValue
  = IntValue Int
  | StringValue String
  | EnumValue Enum

type alias Enum =
  { id: Int
  , label: String
  }

decodeProduct : Decoder Product
decodeProduct =
  map4 Product
    (field "id" int)
    (field "name" string)
    (field "price" int)
    (field "attributes" decodeAttributes)

decodeAttributes : Decoder Attributes
decodeAttributes =
  list decodeAttribute

decodeAttribute : Decoder Attribute
decodeAttribute =
  map4 Attribute
   (field "id" int)
   (field "name" string)
   (field "unit" string)
   (field "value" decodeAttributeValue)

decodeAttributeValue : Decoder AttributeValue
decodeAttributeValue =
  oneOf
    [ Json.Decode.map IntValue int
    , Json.Decode.map StringValue string
    , Json.Decode.map EnumValue decodeEnumValue
    ]

decodeEnumValue : Decoder Enum
decodeEnumValue =
  map2 Enum
    (field "id" int)
    (field "label" string)


decodeProduct2 : Decoder Product
decodeProduct2 =
  map4 Product
    (field "id" int)
    (field "name" string)
    (field "price" int)
    (field "attributes" decodeAttributes2)

decodeAttributes2 : Decoder Attributes
decodeAttributes2 =
  list decodeAttribute2

decodeAttribute2 : Decoder Attribute
decodeAttribute2 =
  field "type" string
   |> andThen decodeAttributeValueType
   |> andThen (\attributeValue ->
      map4 Attribute
         (field "id" int)
         (field "name" string)
         (field "unit" string)
         (succeed attributeValue)
   )

decodeAttributeValueType : String -> Decoder AttributeValue
decodeAttributeValueType valueType =
  case valueType of
    "int" ->
      field "value_int" int
        |> Json.Decode.map IntValue

    "string" ->
      field "value_string" string
        |> Json.Decode.map StringValue

    "enum" ->
      map2 Enum
        (field "value_enum_id" int)
        (field "value_enum_label" string)
        |> Json.Decode.map EnumValue

    _ ->
      Json.Decode.fail "Unknown attribute type"


decodeDateFromString : Decoder Date.Date
decodeDateFromString =
  string
    |> andThen (\stringDate ->
      case Date.fromString stringDate of
        Ok date -> Json.Decode.succeed date
        Err reason -> Json.Decode.fail reason
    )

decodeDateFromTimestamp : Decoder Date.Date
decodeDateFromTimestamp =
  oneOf
    [ int
        |> Json.Decode.map toFloat
    , float  ]
    |> Json.Decode.map Date.fromTime
