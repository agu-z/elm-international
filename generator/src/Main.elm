module Main exposing (program)

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Posix.IO as IO exposing (IO, Process)
import Posix.IO.File as File
import Posix.IO.Process as Proc


type alias Locales =
    Dict String PluralRules


localesDecoder : Decoder Locales
localesDecoder =
    D.dict pluralRulesDecoder


type alias PluralRules =
    { zero : Maybe String
    , one : Maybe String
    , two : Maybe String
    , few : Maybe String
    , many : Maybe String
    }


pluralRulesDecoder : Decoder PluralRules
pluralRulesDecoder =
    D.map5 PluralRules
        (ruleCategory "zero")
        (ruleCategory "one")
        (ruleCategory "two")
        (ruleCategory "few")
        (ruleCategory "many")


ruleCategory : String -> Decoder (Maybe String)
ruleCategory name =
    D.field ("pluralRule-count-" ++ name) D.string
        |> D.maybe
        |> D.map
            (Maybe.map String.trim
                >> Maybe.andThen nonEmptyOrNothing
            )


nonEmptyOrNothing : String -> Maybe String
nonEmptyOrNothing str =
    if String.isEmpty str then
        Just str

    else
        Nothing


program : Process -> IO ()
program _ =
    File.contentsOf "cldr-json/cldr-json/cldr-core/supplemental/plurals.json"
        |> IO.exitOnError identity
        |> IO.andThen (D.decodeString localesDecoder >> IO.return >> IO.exitOnError (\_ -> "Failed to parse plurals.json"))
        |> IO.andThen (Debug.toString >> Proc.print)
