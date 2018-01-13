namespace SentimentFS.TextUtilities.Tests

open Expecto.Flip
module Regex =
    open Expecto
    open SentimentFS.TextUtilities.Regex

    [<Tests>]
    let test =
        testList "Regex" [
            testList "Regex" [
                testCase "matched text" <| fun _ ->
                    let dominik = "Dominik"
                    let subject = match dominik with
                                  | Regex "([A-Z])\w+" _ -> true
                                  | _ -> false
                    Expect.isTrue subject "should be matched"
                testCase "not matched text" <| fun _ ->
                    let dominik = "dominik"
                    let subject = match dominik with
                                  | Regex "([A-Z])\w+" _ -> true
                                  | _ -> false
                    Expect.isFalse subject "should be not matched"
            ]
            testList "FirstMatch" [
                testCase "matched text" <| fun _ ->
                    let dominik = "Dominik"
                    let subject = match dominik with
                                  | FirstMatch "([A-Z])\w+" value -> value
                                  | _ -> ""
                    Expect.equal subject "Dominik" "should be matched and equal Dominik"
                testCase "not matched text" <| fun _ ->
                    let dominik = "dominik"
                    let subject = match dominik with
                                  | FirstMatch "([A-Z])\w+" value -> value
                                  | _ -> ""
                    Expect.equal subject "" "should not match"
            ]
            testList "SuffixMatch" [
                testCase "matched text" <| fun _ ->
                    let dominik = "Dominik"
                    let subject = match dominik with
                                  | SuffixMatch "ik" value -> value
                                  | _ -> ""
                    Expect.equal subject "ik" "should be matched and equal ik"
                testCase "not matched text" <| fun _ ->
                    let dominik = "dominik"
                    let subject = match dominik with
                                  | FirstMatch "fdsa" value -> value
                                  | _ -> ""
                    Expect.equal subject "" "should not match"
            ]
        ]

module Text =
    open Expecto
    open SentimentFS.TextUtilities.Text

    [<Tests>]
    let test =
        testList "Text" [
            testList "chop" [
                test "some test" {
                    let subject = "dominik" |> chop 5
                    Expect.equal subject "do" "trimmed word should equal do"
                }
            ]
            testList "skip" [
                test "some test" {
                    let subject = "dominik" |> skip 5
                    Expect.equal subject "ik" "trimmed word should equal ik"
                }
            ]
            testList "skipPrefix" [
                testCase "text is prefix" <| fun _ ->
                    let subject = "dominik" |> skipPrefix "dom"
                    Expect.equal subject "inik" "trimmed word should equal inik"
                testCase "text is not prefix" <| fun _ ->
                    let subject = "dominik" |> skipPrefix "domd"
                    Expect.equal subject "dominik" "trimmed word should be the same"
            ]
            testList "removeSuffix" [
                testCase "text is suffix" <| fun _ ->
                    let subject = "dominik" |> removeSuffix "inik"
                    Expect.equal subject "dom" "trimmed word should equal dom"
                testCase "text is not suffix" <| fun _ ->
                    let subject = "dominik" |> removeSuffix "inifk"
                    Expect.equal subject "dominik" "trimmed word should be the same"
            ]
            testList "replaceSuffix" [
                testCase "text is suffix" <| fun _ ->
                    let subject = "dominik" |> replaceSuffix "inik" "ino"
                    Expect.equal subject "domino" "replaced word should equal domino"
                testCase "text is not suffix" <| fun _ ->
                    let subject = "dominik" |> replaceSuffix "inifk" "i"
                    Expect.equal subject "dominik" "replaced word should be the same"
            ]
            testList "replacePrefix" [
                testCase "text is prefix" <| fun _ ->
                    let subject = "hello world" |> replacePrefix "hello " ""
                    Expect.equal subject "world" "replaced word should equal world"
                testCase "text is not prefix" <| fun _ ->
                    let subject = "world" |> replacePrefix "hello" ""
                    Expect.equal subject "world" "replaced word should be the same"
            ]
            testList "endsWith" [
                testCase "when text ends with any params" <| fun _ ->
                    let subject = endsWith "word" [|"ord"|]
                    Expect.isTrue subject "should be true"
                testCase "when text no ends with any params" <| fun _ ->
                    let subject = endsWith "word" [|"ordsd"|]
                    Expect.isFalse subject "should be false"
            ]
            testList "startsWith" [
                testCase "when text starts with any params" <| fun _ ->
                    let subject = startsWith "word" [|"wor"|]
                    Expect.isTrue subject "should be true"
                testCase "when text no starts with any params" <| fun _ ->
                    let subject = startsWith "word" [|"ordsd"|]
                    Expect.isFalse subject "should be false"
            ]
            testList "tfidf" [
                testList "tf" [
                    testCase "Word doesnt exists in document " <| fun _ ->
                        let document = ["this"; "is"; "a"; "a"; "sample"]
                        let subject = document |> tf "example"
                        Expect.equal subject 0.0 "result should equal 0.0"
                    testCase "Word exists in document " <| fun _ ->
                        let document = ["this"; "is"; "another"; "another"; "example"; "example"; "example"]
                        let subject = document |> tf "example"
                        Expect.isGreaterThan subject 0.0 "result should be greater than 0.0"
                        Expect.floatClose Accuracy.medium subject 0.4285714286 "result should equal 0.4285714286"
                ]
                testList "idf" [
                    testCase "Word doesnt exists in document " <| fun _ ->
                        let document1 = ["this"; "is"; "a"; "a"; "sample"]
                        let document2 = ["this"; "is"; "another"; "another"; "example"; "example"; "example"]
                        let documents = [document1; document2]
                        let subject = idf("this")(documents)
                        Expect.equal subject 0.0 "result should equal 0.0"
                    testCase "Word exists in document " <| fun _ ->
                        let document1 = ["this"; "is"; "a"; "a"; "sample"]
                        let document2 = ["this"; "is"; "another"; "another"; "example"; "example"; "example"]
                        let documents = [document1; document2]
                        let subject = idf("example")(documents)
                        Expect.isGreaterThan subject 0.0 "result should be greater than 0.0"
                        Expect.floatClose Accuracy.medium subject 0.3010299957 "result should equal 0.3010299957"
                ]
                testList "tf-idf" [
                    testCase "Word doesnt exists in document " <| fun _ ->
                        let document1 = ["this"; "is"; "a"; "a"; "sample"]
                        let document2 = ["this"; "is"; "another"; "another"; "example"; "example"; "example"]
                        let documents = [document1; document2]
                        let subject = tf("example")(document1) * idf("example")(documents)
                        Expect.equal subject 0.0 "result should equal 0.0"
                    testCase "Word exists in document " <| fun _ ->
                        let document1 = ["this"; "is"; "a"; "a"; "sample"]
                        let document2 = ["this"; "is"; "another"; "another"; "example"; "example"; "example"]
                        let documents = [document1; document2]
                        let subject = tf("example")(document2) * idf("example")(documents)
                        Expect.isGreaterThan subject 0.0 "result should be greater than 0.0"
                        Expect.floatClose Accuracy.medium subject 0.1290128553 "result should equal 0.1290128553"
                ]
            ]
        ]
