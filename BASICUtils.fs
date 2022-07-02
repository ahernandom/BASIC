module BASICUtils

open System.IO
open BASICLexer
open BASICParser

let parseString text = 
    physicalNumber <- ref 1
    let lexbuf = Lexing.LexBuffer<char>.FromString text
    try 
        start token lexbuf 
    with e ->  
        failwithf "Error in line %d: %s" !physicalNumber e.Message
        
let parseFile filename =
    let text = File.ReadAllText(filename)
    parseString text

let writeFile filename content =
    File.WriteAllText(filename, content)
