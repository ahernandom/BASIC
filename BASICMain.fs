open BASICUtils
open BASICSemantic
open BASICCodeGenerator
 
[<EntryPoint>]
let main (args : string[]) =        
    printfn "BASIC compiler, version 1.0"
    printfn "Copyright (C) 2012 by A. Hernando, A. Salazar, S. Aguilar, ITESM CEM."
    try
        if args.Length <> 2 then
            failwith "Please specify input and output file names." 
        let parseTree = parseFile args.[0]
        printfn "Syntax OK"
        let symbolTable = semanticAnalysis parseTree
        let listsTable = getListsTable 0
        let tablesTable = getTablesTable 0
        let functionsTable = getFunctionsTable 0
        printfn "Semantics OK"
        printfn "Lists %A" listsTable
        let code = generateCode symbolTable listsTable tablesTable functionsTable parseTree
        writeFile args.[1] code
        printfn "CIL code generated"
        0
    with
        | Failure(msg) ->
            printfn "%s" msg
            1
        | e -> 
            printfn "%s" e.Message
            1
