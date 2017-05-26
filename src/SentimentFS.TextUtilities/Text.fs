namespace SentimentFS.TextUtilities

module String =
    open System.Text.RegularExpressions
    
    let replace (pattern: string)(replacement: string)(word: string) =
        Regex.Replace(word, pattern, replacement)
        
    let split(pattern: char)(text: string) =
        text.Split(pattern)
    
    let toLower(word: string) =
        word.ToLower()