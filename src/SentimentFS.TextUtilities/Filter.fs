namespace SentimentFS.TextUtilities

module Filter =
    let filterOut(filterList: string list)(list: string list) =
        list |> List.filter(fun x -> not (filterList |> List.exists(fun y -> x = y)))

    let filterOutSeq(filterSeq: string seq)(list: string seq) =
        filterOut(filterSeq |> Seq.toList)(list |> Seq.toList) |> List.toSeq
