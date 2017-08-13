namespace SentimentFS.TextUtilities.Tests

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
        ]
