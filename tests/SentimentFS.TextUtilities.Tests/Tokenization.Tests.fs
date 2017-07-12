namespace SentimentFS.TextUtilities.Tests

module Tokenizer =
    open SentimentFS.TextUtilities.Tokenizer
    open Expecto

    [<Tests>]
    let tests =
        testList "Tokenizer" [
            testList "tokenize" [
                testCase "Hello world. And !@#$%^&*(*)" <| fun _ ->
                    let subject = tokenize "Hello world. And !@#$%^&*(*)"
                    Expect.equal subject ["hello"; "world"; "and"] "should equal"
                testCase "Friends, Romans, Countrymen, lend me your ears" <| fun _ ->
                    let subject = tokenize "Friends, Romans, Countrymen, lend me your ears"
                    Expect.equal subject ["friends"; "romans"; "countrymen"; "lend"; "me"; "your"; "ears"] "should equal"
            ]
            testList "wordsSequence" [
                test "words" {
                    let stopWords = """a aren't because                         home"""
                    let subject = stopWords |> wordsSequence |> Seq.toList
                    Expect.equal subject (["a"; "aren't"; "because"; "home"]) ""
                }
            ]
        ]
