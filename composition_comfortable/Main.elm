module Main exposing (..)

import Html exposing (..)
import Html.Events as Events
import Html.Attributes as Attrs
import Type.User exposing (User(..))
import Component.Auth
import Component.Question


init : ( Model, Cmd Msg )
init =
  ( initModel, Cmd.none )


type Msg
  = OpenPopup

  -- Hide authentication component messages
  | AuthMsg Component.Auth.Msg

  -- Hide question component messages
  | QuestionMsg Component.Question.Msg


type alias Model =
  { user: User
  , ui: Maybe Ui   -- Popup is not open if value equals Nothing
  }


type Ui

  -- Hide authentication component model
  = AuthUi Component.Auth.Model

  -- Hide question component model
  | QuestionUi Component.Question.Model


initModel : Model
initModel =
  { user = Anonymous
  , ui = Nothing
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
  case (msg, model.ui) of

  -- Anonymous user message handling section
    (OpenPopup, Nothing) ->
      case Component.Auth.init model.user of
        (authModel, commands, Just (Component.Auth.Authenticated userData)) ->
          let
            (questionModel, questionCommands, _) = Component.Question.init userData
          in
            ( { model | ui = Just <| QuestionUi questionModel, user = User userData }, Cmd.batch [Cmd.map AuthMsg commands, Cmd.map QuestionMsg questionCommands] )

        (authModel, commands, _) ->
          ( { model | ui = Just <| AuthUi authModel }, Cmd.map AuthMsg commands )

  -- Handle authenticate component messages
    (AuthMsg authMsg, Just (AuthUi authModel)) ->
      case Component.Auth.update authMsg authModel of
        (_, commands, Just (Component.Auth.Authenticated userData)) ->
          let
            (questionModel, questionCommands, _) = Component.Question.init userData
          in
            ( { model | ui = Just <| QuestionUi questionModel, user = User userData }, Cmd.batch [Cmd.map AuthMsg commands, Cmd.map QuestionMsg questionCommands] )

        (newAuthModel, commands, _) ->
          ( { model | ui = Just <| AuthUi newAuthModel }, Cmd.map AuthMsg commands )

   -- Handle question component messages
    (QuestionMsg questionMsg, Just (QuestionUi questionModel)) ->
      case Component.Question.update questionMsg questionModel of
        (_, commands, Just (Component.Question.Saved record)) ->
          ( { model | ui = Nothing }, Cmd.map QuestionMsg commands )

        (newQuestionModel, commands, _) ->
          ( { model | ui = Just <| QuestionUi newQuestionModel }, Cmd.map QuestionMsg commands )

  -- ignore all other cases
    _ ->
      ( model, Cmd.none )


view : Model -> Html Msg
view model =
  case model.ui of
    Nothing ->
      div []
        [ div []
          [ button
              [ Events.onClick OpenPopup ]
              [ text "Open popup" ]
          ]
        ]

    Just (AuthUi authModel) ->
      Component.Auth.view authModel
        |> Html.map AuthMsg

    Just (QuestionUi questionModel) ->
       Component.Question.view questionModel
         |> Html.map QuestionMsg