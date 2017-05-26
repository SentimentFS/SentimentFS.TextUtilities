namespace SentimentFS.TextUtilities.Tests

module Filter =
    open Expecto
    open SentimentFS.TextUtilities
    
    [<Tests>]
    let filter = 
        testList "Filter" [
            testList "filterOut" [
                testCase "delete when elements exist in  filter list" <| fun _ -> 
                    let subject = ["A"; "B"; "C"] |> Filter.filterOut ["C"]
                    Expect.equal subject ["A";"B"] "should equal ['A';'B']"
                testCase "delete when elements no exist in filter list" <| fun _ -> 
                    let subject = ["A"; "B"; "C"] |> Filter.filterOut ["D"]
                    Expect.equal subject ["A";"B";"C"] "should equal ['A';'B';'C']"
            ]
        ]