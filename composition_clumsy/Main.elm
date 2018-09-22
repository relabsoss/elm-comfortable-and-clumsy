module Main exposing (..)

import Html exposing (..)
import Html.Events as Events
import Html.Attributes as Attrs
import Http


init : ( Model, Cmd Msg )
init =
  ( initModel, Cmd.none )


type User
  = Anonymous
  | User UserData


type alias UserData = String


type Msg
  = OpenPopup
  | LoginTyped String
  | PasswordTyped String
  | Login
  | QuestionTyped String
  | SendQuestion


type LoginError
  = WrongLogin
  | WrongPassword


type alias Model =
  { user: User
  , ui: Maybe Ui   -- Popup is not open if value equals Nothing
  , login: String
  , password: String
  , question: String
  , message: String
  }


type Ui
  = LoginUi      -- Popup shown with authentication form
  | QuestionUi   -- Popup shown with textarea to leave user question


initModel : Model
initModel =
  { user = Anonymous
  , ui = Nothing
  , login = ""
  , password = ""
  , question = ""
  , message = ""
  }


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case (msg, model.user) of

  -- Anonymous user message handling section
    (OpenPopup, Anonymous) ->
      ( { model | ui = Just LoginUi, message = "" }, Cmd.none)

    (LoginTyped value, Anonymous) ->
      ( { model | login = value }, Cmd.none )

    (PasswordTyped value, Anonymous) ->
      ( { model | password = value }, Cmd.none )

    (Login, Anonymous) ->
      ( { model | ui = Just QuestionUi, user = User model.login, login = "", password = "" }, Cmd.none )

  -- Authenticated user message handling section
    (OpenPopup, User userName) ->
      ( { model | ui = Just QuestionUi, message = "" }, Cmd.none)

    (QuestionTyped value, User userName) ->
       ( { model | question = value }, Cmd.none )

    (SendQuestion, User userName) ->
       ( { model | ui = Nothing, question = "", message = "Question sended" }, Cmd.none )

  -- ignore all other cases
    _ ->
      ( model, Cmd.none )


view : Model -> Html Msg
view model =
  case model.ui of
    Nothing ->
      div []
        [ text model.message
        , div []
          [ button
              [ Events.onClick OpenPopup ]
              [ text "Open popup" ]
          ]
        ]

    Just LoginUi ->
      viewLogin model

    Just QuestionUi ->
      case model.user of
        Anonymous ->
          viewLogin model

        User userName ->
          viewQuestion userName model


viewLogin : Model -> Html Msg
viewLogin model =
  table []
    [ tr []
        [ td []
            [ input [ Events.onInput LoginTyped, Attrs.placeholder "Login" ] [] ]
        ]

    , tr []
        [ td []
            [ input [ Events.onInput PasswordTyped, Attrs.placeholder "Password" ] []  ]
        ]

    , tr []
        [ td [] [ button [ Events.onClick Login ] [ text "Authenticate" ] ]
        ]
    ]


viewQuestion : String -> Model -> Html Msg
viewQuestion userName model =
  div []
    [ div []
        [ text <| "User: " ++ userName ]
    , div []
        [ textarea [ Events.onInput QuestionTyped, Attrs.placeholder "Question" ] [] ]
    , button [ Events.onClick SendQuestion ] [ text "Send" ]
    ]