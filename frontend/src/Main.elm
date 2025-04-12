module Main exposing (..)

import Api
import Browser
import Browser.Navigation as Nav
import Bulma.Classes as B
import Iso8601
import Entity.Authority as Authority exposing (AuthorityEntity)
import Entity.Authority.Derive as Authority
import Entity.User as User exposing (UserEntity)
import Entity.User.Derive as User
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import RemoteData exposing (RemoteData(..), WebData)
import Url
import Util exposing (ListElement(..))



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , count : Int
    , authorities : WebData (List AuthorityEntity)
    , users : WebData (List UserEntity)
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , url = url
      , count = 0
      , authorities = NotAsked
      , users = NotAsked
      }
    , Cmd.none
    )


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | Increment
    | Decrement
    | GetAuthorities
    | GotAuthorities (Result Http.Error (List AuthorityEntity))
    | GetUsers
    | GotUsers (Result Http.Error (List UserEntity))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )

        GetAuthorities ->
            case model.authorities of
                Loading ->
                    ( model, Cmd.none )

                _ ->
                    ( { model | authorities = Loading }, Api.getAuthorities GotAuthorities )

        GotAuthorities res ->
            ( { model | authorities = RemoteData.fromResult res }, Cmd.none )

        GetUsers ->
            case model.users of
                Loading ->
                    ( model, Cmd.none )

                _ ->
                    ( { model | users = Loading }, Api.getUsers GotUsers )

        GotUsers res ->
            ( { model | users = RemoteData.fromResult res }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Counter"
    , body =
        [ div [ class B.box ]
            [ button [ class B.button, onClick Decrement ] [ text "-" ]
            , div [] [ text (String.fromInt model.count) ]
            , button [ class B.button, onClick Increment ] [ text "+" ]
            ]
        , section [ class B.section ]
            [ button
                (Util.buildList
                    [ Single <| class B.button
                    , Single <| class B.isPrimary
                    , Single <| onClick GetAuthorities
                    , SingleIf (RemoteData.isLoading model.authorities) (\_ -> class B.isLoading)
                    ]
                )
                [ text "Get Authorities" ]
            , tableAuthorities model.authorities
            ]
        , section [ class B.section ]
            [ button
                (Util.buildList
                    [ Single <| class B.button
                    , Single <| class B.isPrimary
                    , Single <| onClick GetUsers
                    , SingleIf (RemoteData.isLoading model.users) (\_ -> class B.isLoading)
                    ]
                )
                [ text "Get Users" ]
            , tableUsers model.users
            ]
        ]
    }


tableAuthorities : WebData (List AuthorityEntity) -> Html Msg
tableAuthorities wAuthorities =
    table [ class B.table ]
        [ thead []
            [ tr []
                [ td [] [ text "ID" ]
                , td [] [ text "Label" ]
                , td [] [ text "Level" ]
                , td [] [ text "Created At" ]
                , td [] [ text "Updated At" ]
                ]
            ]
        , tbody [] <|
            case wAuthorities of
                NotAsked ->
                    []

                Loading ->
                    [ tr [] [ td [] [ text "Loading..." ] ] ]

                Failure err ->
                    [ tr [] [ td [] [ text <| Api.errorText err ] ] ]

                Success authorities ->
                    List.map
                        (\{ id, meta, payload } ->
                            tr []
                                [ td [] [ text <| Authority.fromId id ]
                                , td [] [ text payload.label ]
                                , td [] [ text (String.fromInt payload.level) ]
                                , td [] [ text <| Iso8601.fromTime meta.createdAt ]
                                , td [] [ text <| Iso8601.fromTime meta.updatedAt ]
                                ]
                        )
                        authorities
        ]


tableUsers : WebData (List UserEntity) -> Html Msg
tableUsers wUsers =
    table [ class B.table ]
        [ thead []
            [ tr []
                [ td [] [ text "ID" ]
                , td [] [ text "Last Name" ]
                , td [] [ text "First Name" ]
                , td [] [ text "Created At" ]
                , td [] [ text "Updated At" ]
                ]
            ]
        , tbody [] <|
            case wUsers of
                NotAsked ->
                    []

                Loading ->
                    [ tr [] [ td [] [ text "Loading..." ] ] ]

                Failure err ->
                    [ tr [] [ td [] [ text <| Api.errorText err ] ] ]

                Success users ->
                    List.map
                        (\{ id, meta, payload } ->
                            tr []
                                [ td [] [ text <| User.fromId id ]
                                , td [] [ text payload.lastName ]
                                , td [] [ text payload.firstName ]
                                , td [] [ text <| Iso8601.fromTime meta.createdAt ]
                                , td [] [ text <| Iso8601.fromTime meta.updatedAt ]
                                ]
                        )
                        users
        ]
