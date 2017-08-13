namespace SentimentFS.TextUtilities

module Text =
    open System
    open System.Text.RegularExpressions

    let chop count (text:string) =
        text.Remove(text.Length-count)

    let skip count (text:string) =
        text.Substring(count, text.Length-count)
    let skipPrefix prefix (text:string) =
        if text.StartsWith prefix then
            skip prefix.Length text
        else
            text
    let removeSuffix suffix (text:string) =
        if text.EndsWith suffix then
            chop suffix.Length text
        else
            text

    let replace (pattern: string)(replacement: string)(word: string) =
        Regex.Replace(word, pattern, replacement)

    let split([<ParamArray>]patterns: char array)(text: string) =
        text.Split patterns

    let toLower(word: string) =
        word.ToLower()
