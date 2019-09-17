import Browser
import Html exposing (Html, div, text)

main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model = 
  { text : String
  }

init : Model
init = Model "Hello World"

-- UPDATE

type Msg 
  = Basic

update : Msg -> Model -> Model
update msg model =
  case msg of
    Basic -> model

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ text model.text]
