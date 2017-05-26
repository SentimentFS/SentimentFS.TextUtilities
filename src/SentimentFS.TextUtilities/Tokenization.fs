namespace SentimentFS.TextUtilities
module Tokenizer =
    open SentimentFS.TextUtilities.String
    let tokenize(word: string) =
        word 
            |> split(' ') 
            |> Array.map(replace ("\W") ("") >> toLower) 
            |> Array.filter(fun x -> x <> "") 
            |> Array.toList