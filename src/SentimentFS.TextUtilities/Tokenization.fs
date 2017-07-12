namespace SentimentFS.TextUtilities
module Tokenizer =
    open SentimentFS.TextUtilities.String
    open System

    let tokenize(word: string) =
        word
            |> split([|' '|])
            |> Array.map(replace ("\W") ("") >> toLower)
            |> Array.filter(String.IsNullOrEmpty >> not)
            |> Array.toList

    let wordsSequence(word: string): string seq =
        word |> split [|' '; '\n'|] |> Array.filter(String.IsNullOrEmpty >> not) |> Array.map(fun x -> x |> replace("\n")(String.Empty) |> replace("\r")(String.Empty) |> replace("\t")(String.Empty)  ) |> Array.toSeq
