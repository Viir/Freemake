module Backend.Main exposing
    ( State
    , backendMain
    )

import CompilationInterface.ElmMake
import ElmFullstack


type alias State =
    ()


backendMain : ElmFullstack.BackendConfig ()
backendMain =
    { init = ( (), [] )
    , subscriptions = subscriptions
    }


subscriptions : State -> ElmFullstack.BackendSubs State
subscriptions _ =
    { httpRequest = updateForHttpRequestEvent
    , posixTimeIsPast = Nothing
    }


updateForHttpRequestEvent : ElmFullstack.HttpRequestEventStruct -> State -> ( State, ElmFullstack.BackendCmds State )
updateForHttpRequestEvent httpRequestEvent state =
    let
        staticContentHttpHeaders contentType =
            { cacheMaxAgeMinutes = Just (60 * 4)
            , contentType = contentType
            }

        httpResponseOkWithBodyAsBase64 bodyAsBase64 { cacheMaxAgeMinutes, contentType } =
            let
                cacheHeaders =
                    case cacheMaxAgeMinutes of
                        Nothing ->
                            []

                        Just maxAgeMinutes ->
                            [ { name = "Cache-Control"
                              , values = [ "public, max-age=" ++ String.fromInt (maxAgeMinutes * 60) ]
                              }
                            , { name = "Content-Type"
                              , values = [ contentType ]
                              }
                            ]
            in
            { httpRequestId = httpRequestEvent.httpRequestId
            , response =
                { statusCode = 200
                , bodyAsBase64 = bodyAsBase64
                , headersToAdd = cacheHeaders
                }
            }
    in
    ( state
    , [ ElmFullstack.RespondToHttpRequest
            (httpResponseOkWithBodyAsBase64
                (Just CompilationInterface.ElmMake.elm_make____src_Main_elm.html.base64)
                (staticContentHttpHeaders "text/html")
            )
      ]
    )
