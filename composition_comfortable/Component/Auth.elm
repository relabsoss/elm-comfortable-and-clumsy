module Component.Auth exposing (..)

import Http
import Task
import Html
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Encode
import Type.User exposing (User(..), UserData, decodeUserData)


type alias Model =
  { login: String
  , password: String
  }


type Msg
  = LoginTyped String
  | PasswordTyped String
  | Login
  | RequestResult (Result Http.Error UserData)


type Return
  = Authenticated UserData


init : User -> (Model, Cmd Msg, Maybe Return)
init user =
  case user of
    Anonymous ->
      ( initModel, Cmd.none, Nothing )

    User userData ->
      ( initModel, Cmd.none, Just <| Authenticated userData )


initModel : Model
initModel =
  { login = ""
  , password = ""
  }


update : Msg -> Model -> (Model, Cmd Msg, Maybe Return)
update msg model =
  case msg of
  -- Save user input
    LoginTyped value ->
      ({ model | login = value }, Cmd.none, Nothing )

    PasswordTyped value ->
      ({ model | password = value }, Cmd.none, Nothing )

  -- Authenticate user on login button clicked
    Login ->
      ( model, authenticate model, Nothing )

  -- Handle API result
    RequestResult (Ok userData) ->
      ( model, Cmd.none, Just <| Authenticated userData )

    RequestResult (Err reason) ->
      ( model, Cmd.none, Nothing )


view : Model -> Html.Html Msg
view model =
  Html.div []
      [ Html.div []
          [ Html.input [ Events.onInput LoginTyped, Attrs.placeholder "Login" ] []
          ]

      , Html.div []
          [ Html.input [ Events.onInput PasswordTyped, Attrs.placeholder "Password" ] []
          ]

      , Html.div []
          [ Html.button [ Events.onClick Login ]
              [ Html.text "Authenticate"
              ]
          ]
      ]


authenticate : Model -> Cmd Msg
authenticate model =
  Http.post
    authenticateUrl
    (Http.jsonBody <| authenticateBody model)
    decodeUserData
    |> Http.toTask
    |> Task.attempt RequestResult


authenticateUrl : String
authenticateUrl = "/composition_comfortable/login.json"


authenticateBody : Model -> Json.Encode.Value
authenticateBody { login, password } =
  Json.Encode.object
    [ ("login", Json.Encode.string login )
    , ("password", Json.Encode.string password )
    ]