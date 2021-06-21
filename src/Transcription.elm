module Transcription exposing (Transcription, renderTranscription, transcriptionDecoder)

import Json.Decode as Decode exposing (Decoder, index, int, list, map, oneOf, string)
import Json.Decode.Pipeline exposing (hardcoded, required)
import List.Extra as List



{-
          ___        ______
         / \ \      / / ___|
        / _ \ \ /\ / /\___ \
       / ___ \ V  V /  ___) |
      /_/   \_\_/\_/  |____/

       _____                              _       _   _
      |_   _| __ __ _ _ __  ___  ___ _ __(_)_ __ | |_(_) ___  _ __  ___
        | || '__/ _` | '_ \/ __|/ __| '__| | '_ \| __| |/ _ \| '_ \/ __|
        | || | | (_| | | | \__ \ (__| |  | | |_) | |_| | (_) | | | \__ \
        |_||_|  \__,_|_| |_|___/\___|_|  |_| .__/ \__|_|\___/|_| |_|___/
                                           |_|
   These types and functions help with decoding a transcription from aws and for cue scripts
-}


renderTranscription : Transcription -> String
renderTranscription transcription =
    let
        speakerSectionView utterance =
            transcription.items
                |> List.dropWhile
                    (\phrase ->
                        phrase.start_time < utterance.start_time
                    )
                |> List.takeWhile
                    (\phrase ->
                        phrase.end_time <= utterance.end_time
                    )
                |> List.map .phrase
                |> String.join " "
                |> List.singleton
                |> (++) [ utterance.speaker_label ]
    in
    transcription.speaker_labels
        |> List.concatMap speakerSectionView
        |> String.join "\n"


type alias Transcription =
    { items : List Phrase
    , speaker_labels : List Utterance
    , jobName : String
    }


type alias Phrase =
    { phrase : String, start_time : Float, end_time : Float }


type alias Utterance =
    { start_time : Float, end_time : Float, speaker_label : String }


transcriptionDecoder : Decode.Decoder Transcription
transcriptionDecoder =
    let
        itemDecoder =
            Decode.map4
                (\s e f t ->
                    { start_time = s
                    , end_time = e
                    , content = f
                    , type_ = t
                    }
                )
                (Decode.maybe (Decode.field "start_time" floatAsStringDecoder))
                (Decode.maybe (Decode.field "end_time" floatAsStringDecoder))
                (Decode.field "alternatives" (Decode.index 0 (Decode.field "content" Decode.string)))
                (Decode.field "type" Decode.string)

        itemsToPhrases items =
            let
                itemToPhraseHelper thisItem phrasesSoFar =
                    case ( thisItem.type_, thisItem.start_time, thisItem.end_time ) of
                        ( "pronunciation", Just start, Just end ) ->
                            Phrase thisItem.content start end :: phrasesSoFar

                        _ ->
                            case phrasesSoFar of
                                lastPhraseAdded :: restOfPhrases ->
                                    { lastPhraseAdded
                                        | phrase = lastPhraseAdded.phrase ++ thisItem.content
                                    }
                                        :: restOfPhrases

                                [] ->
                                    []
            in
            List.foldl itemToPhraseHelper [] items
                |> List.reverse

        speakerLabelDecoderHelper =
            Decode.succeed identity
                |> required "segments"
                    (Decode.list
                        (Decode.succeed Utterance
                            |> required "start_time" floatAsStringDecoder
                            |> required "end_time" floatAsStringDecoder
                            |> required "speaker_label" Decode.string
                        )
                    )

        speakerLabelDecoder =
            Decode.maybe (Decode.field "speaker_labels" speakerLabelDecoderHelper)
                |> Decode.andThen
                    (\maybeSpeakerLabels ->
                        case maybeSpeakerLabels of
                            Just labels ->
                                Decode.succeed labels

                            Nothing ->
                                -- AWS Transcribe removes `results.speaker_labels` for undiarized / 1 person
                                Decode.succeed
                                    [ { start_time = 0

                                      -- JSON doesn't allow encoding Infinity or NaN, but we get around that by
                                      -- encoding floats as strings beforehand
                                      , end_time = 1 / 0
                                      , speaker_label = "spk_0"
                                      }
                                    ]
                    )
    in
    Decode.map3 Transcription
        (Decode.at [ "results", "items" ]
            (Decode.list itemDecoder
                |> Decode.map itemsToPhrases
            )
        )
        (Decode.at [ "results" ] speakerLabelDecoder)
        (Decode.at [ "jobName" ] Decode.string)


floatAsStringDecoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case String.toFloat string of
                    Nothing ->
                        Decode.fail <| "Unable to parse float from string!" ++ string

                    Just f ->
                        Decode.succeed f
            )
