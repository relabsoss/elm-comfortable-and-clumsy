module Type.Question exposing (..)

import Json.Encode
import Json.Decode


type alias Question = String


type alias Record =
  { id: Int
  , question: String
  }


toString : Question -> String
toString question = question


decodeQuestion : Json.Decode.Decoder Question
decodeQuestion = Json.Decode.string


encodeQuestion : Question -> Json.Encode.Value
encodeQuestion = Json.Encode.string


decodeRecord : Json.Decode.Decoder Record
decodeRecord =
  Json.Decode.map2 Record
    ( Json.Decode.field "id" Json.Decode.int )
    ( Json.Decode.field "question" Json.Decode.string )