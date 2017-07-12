namespace SentimentFS.TextUtilities

module String =
    open System
    open System.Text.RegularExpressions

    let replace (pattern: string)(replacement: string)(word: string) =
        Regex.Replace(word, pattern, replacement)

    let split([<ParamArray>]patterns: char array)(text: string) =
        text.Split patterns

    let toLower(word: string) =
        word.ToLower()
