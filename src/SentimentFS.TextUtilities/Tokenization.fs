namespace SentimentFS.TextUtilities
module Tokenizer =
    open SentimentFS.TextUtilities.String
    open System

    let tokenize =
        split [|' '|]
            >> Array.map(replace ("\W") ("") >> toLower)
            >> Array.filter(String.IsNullOrEmpty >> not)
            >> Array.toList

    let wordsSequence =
        split [|' '; '\n'|]
            >> Array.filter(String.IsNullOrEmpty >> not)
            >> Array.map(replace("\n")(String.Empty) >> replace("\r")(String.Empty)
            >> replace("\t")(String.Empty))
            >> Array.toSeq
