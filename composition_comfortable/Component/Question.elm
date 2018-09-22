module Component.Question exposing (..)

import Http
import Task
import Html
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Encode
import Type.User exposing (UserData)
import Type.Question exposing (Question, encodeQuestion, Record, decodeRecord)


type alias Model =
  { userData: UserData
  , question: Question
  }


type Msg
  = QuestionTyped String
  | Save
  | RequestResult (Result Http.Error Record)


type Return
  = Saved Record


init : UserData -> (Model, Cmd Msg, Maybe Return)
init userData =
  ( initModel userData, Cmd.none, Nothing )


initModel : UserData -> Model
initModel userData =
  { userData = userData
  , question = ""
  }


update : Msg -> Model -> (Model, Cmd Msg, Maybe Return)
update msg model =
  case msg of
  -- Save user input
    QuestionTyped value ->
      ({ model | question = value }, Cmd.none, Nothing )

  -- Save question on button clicked
    Save ->
      ( model, saveQuestion model, Nothing )

  -- Handle API result
    RequestResult ( Ok record ) ->
      ( model, Cmd.none, Just <| Saved record )

    RequestResult ( Err reason ) ->
      ( model, Cmd.none, Nothing )


view : Model -> Html.Html Msg
view { userData, question } =
  Html.div []
      [ Html.div []
          [ Html.text <| "User: " ++ userData.name
          ]

      , Html.div []
          [ Html.textarea
              [ Events.onInput QuestionTyped
              , Attrs.placeholder "Question"
              , Attrs.value <| Type.Question.toString question
              ] []
          ]

      , Html.button [ Events.onClick Save ]
          [ Html.text "Send"
          ]
      ]


saveQuestion : Model -> Cmd Msg
saveQuestion model =
  Http.post
    questionUrl
    (Http.jsonBody <| questionBody model)
    decodeRecord
    |> Http.toTask
    |> Task.attempt RequestResult


questionUrl : String
questionUrl = "/composition_comfortable/questions.json"


questionBody : Model -> Json.Encode.Value
questionBody { userData, question } =
  Json.Encode.object
    [ ("user_id", Json.Encode.int userData.id )
    , ("question", encodeQuestion question )
    ]