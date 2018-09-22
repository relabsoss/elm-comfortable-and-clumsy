module Type.User exposing (..)

import Json.Decode


type User
  = Anonymous
  | User UserData


type alias UserData =
  { id: Int
  , name: String
  }


decodeUserData : Json.Decode.Decoder UserData
decodeUserData =
  Json.Decode.map2 UserData
    ( Json.Decode.field "id" Json.Decode.int )
    ( Json.Decode.field "name" Json.Decode.string )
