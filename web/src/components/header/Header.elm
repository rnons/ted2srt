module Components.Header.Header exposing (view)

import Html exposing (..)
import Components.SearchForm.SearchForm as SearchForm


view : Html msg
view =
    div []
        [ SearchForm.view
        ]
