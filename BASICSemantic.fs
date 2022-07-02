module BASICSemantic

open BASICAST
open BASICUtils

let mutable scalarsTable = Set.empty
let mutable listsTable = Set.empty
let mutable tablesTable = Set.empty
let mutable functionsTable = Set.empty
let mutable labelsList = Set.empty

let mutable forStack = []
let mutable current = 0
let mutable isData = false
let mutable hasEnd = 0
let mutable funDef = 0

let mutable dimmed = Map.empty

exception IllegalNumberException of string
exception NotDefinedFunction of string
    
let semanticError lineNum label message =
    failwithf "Error in line %d (label %s): %s." lineNum label message

let addScalarIfNotExists name =
    if(Set.contains name scalarsTable) = false then
        scalarsTable <- scalarsTable.Add(name)
                
let addListIfNotExists name size =
    for elem in listsTable do
        let n,s = elem
        if (name = n) = true then
            listsTable <- listsTable.Remove((n,s))
    listsTable <- listsTable.Add((name,size))
                
let addTableIfNotExists name size size2 =
    for elem in tablesTable do
        let n,s,s2 = elem
        if (name = n) = true then
            tablesTable <- tablesTable.Remove((n,s,s2))
    tablesTable <- tablesTable.Add((name,size,size2))
                
let addFunctionIfNotExists fnName arg expr =
    for Function(name,a,e) in functionsTable do
        if (name = fnName) = true then
            printfn "Warning: the function %A is already declared, using last one." fnName
    functionsTable <- functionsTable.Add(Function (fnName, arg, expr))        
            
let checkAndSaveNumber number =
    try
        ignore(float(number))
    with _ ->
        raise (IllegalNumberException((sprintf "Illegal constant (float number not representable) : '%s'." number)))

let checkDimNumber number =
    try
        ignore(int(number))
    with _ ->
        raise (IllegalNumberException((sprintf "Illegal constant : '%s'." number)))
                
let checkNumber number =
    try
        ignore(float(number))
    with _ ->
        raise (IllegalNumberException((sprintf "Illegal constant (float number not representable) : '%s'." number)))
            
let rec checkExpr expr =
    match expr with
        | Variable (lineNum, name) ->
            addScalarIfNotExists name
        | List (lineNum, name, expr) ->
            if (Map.isEmpty dimmed) = true || (Map.find ("$"+name) dimmed) = 1 then
                addListIfNotExists name 11
            checkExpr expr
        | Table (lineNum, name, expr, expr2) ->
            if (Map.isEmpty dimmed) = true || (Map.find ("%"+name) dimmed) = 1 then
                addTableIfNotExists name 11 11
            checkExpr expr
            checkExpr expr2
        | Number(lineNum, n) ->
            checkNumber n
        | Fn(name, expr, arg) ->
            for Function(n,e,a) in functionsTable do
                if (name = n) = true then
                    funDef <- 1
            if funDef = 0 then
                raise (NotDefinedFunction((sprintf "You have not defined the function: '%s'." name)))
            match expr with
            | Some(e) ->
                checkExpr e
            | None -> ()
        | Math(name,expr) ->
            checkExpr expr
        | Plus(lineNum, expr, expr2) ->
            checkExpr expr
            checkExpr expr2
        | Minus(lineNum, expr, expr2) ->
            checkExpr expr
            checkExpr expr2
        | Times(lineNum, expr, expr2) ->
            checkExpr expr
            checkExpr expr2
        | Div(lineNum, expr, expr2) ->
            checkExpr expr
            checkExpr expr2
        | Pow(lineNum, expr, expr2) ->
            checkExpr expr
            checkExpr expr2
        | _ -> ()

let checkDim expr = 
    match expr with
    | ListDim(name,size) -> 
        checkDimNumber size
        for listElem in listsTable do
            let n, s = listElem
            if (n = name) = true then
                printfn "Warning: Repeated use of DIM with variable %s. Last one will be used." name
        dimmed <- Map.add ("$"+name) 0 dimmed
        addListIfNotExists name (int(size)+1)
    | TableDim(name,size,size2) ->
        checkDimNumber size
        checkDimNumber size2
        for tableElem in tablesTable do
            let n, s, s2 = tableElem
            if (n = name) = true then
                printfn "Warning: Repeated use of DIM with variable %s. Last one will be used." name
        dimmed <- Map.add ("%"+name) 0 dimmed
        addTableIfNotExists name (int(size)+1) (int(size2)+1)
    | _ -> ()
    
let checkPrint nodeP =
    match nodeP with
    | PExpr (expr) ->
        checkExpr(expr)
    | _ -> ()
        
let checkInt lineNum label =
    try
        int(label)
    with formatException -> semanticError lineNum label (sprintf "Label '%s'is not an integer." label)

let checkLabel lineNum label =
    let n = checkInt lineNum label
    if(Set.contains n labelsList) = false then
        if n > current then
            labelsList <- labelsList.Add(n)
            current <- n
        else
            semanticError lineNum label (sprintf "Labels must be in ascending order.")
    else
        semanticError lineNum label (sprintf "Label '%s' is repeated." label)
    
let analyze stmt =
    match stmt with
    | Let (lineNum, label, varName, expr) ->
        checkLabel lineNum label
        match varName with
        | Variable (lineNum, name) ->
            addScalarIfNotExists name
        | List (lineNum, name, _) ->
            if (Map.isEmpty dimmed) = true || (Map.find ("$"+name) dimmed) = 1 then
                addListIfNotExists name 11
        | Table (lineNum, name, _, _) ->
            if (Map.isEmpty dimmed) = true || (Map.find ("%"+name) dimmed) = 1 then
                addTableIfNotExists name 11 11
        | _ -> ()
        try
            List.iter (fun e -> checkExpr e) [expr]
        with
            | NotDefinedFunction(msg) -> semanticError lineNum label msg
            | IllegalNumberException(msg) -> semanticError lineNum label msg
    | Read (lineNum, label, exprs) ->
        checkLabel lineNum label
        if isData = false then
            semanticError lineNum label "If there is a READ, there must be one or more DATA."
        try
            List.iter (fun expr -> checkExpr expr) exprs
        with
            | NotDefinedFunction(msg) -> semanticError lineNum label msg
            | IllegalNumberException(msg) -> semanticError lineNum label msg
    | Data (lineNum, label, numbers) ->
        checkLabel lineNum label
        try
            List.iter (fun number -> checkAndSaveNumber number) numbers
        with 
            | IllegalNumberException(msg) -> 
                failwithf "Error in line %d (label %s) : %s." lineNum label msg
    | Print (lineNum, label, nodesP) ->
        checkLabel lineNum label
        try
            List.iter (fun nodeP -> checkPrint nodeP) nodesP
        with
            | NotDefinedFunction(msg) -> semanticError lineNum label msg
            | IllegalNumberException(msg) -> semanticError lineNum label msg
    | Goto (lineNum, label, gotoLine) ->
        checkLabel lineNum label
        try
            ignore(int(gotoLine))
        with
            _ -> semanticError lineNum label (sprintf "GOTO line number '%s' is not an integer." gotoLine)
    | If (lineNum, label, expr1, relation, expr2 , thenLine) ->
        checkLabel lineNum label
        try
            List.iter (fun e -> checkExpr e) [expr1]
            List.iter (fun e -> checkExpr e) [expr2]
        with
            | NotDefinedFunction(msg) -> semanticError lineNum label msg
            | IllegalNumberException(msg) -> semanticError lineNum label msg
    | For (lineNum, label, unsubs, exprStart, exprEnd, exprStep) ->
        checkLabel lineNum label
        match unsubs with
        | Variable (lineNum, name) ->
            addScalarIfNotExists name
        | List (lineNum, name, _) ->
            if (Map.isEmpty dimmed) = true || (Map.find ("$"+name) dimmed) = 1 then
                addListIfNotExists name 11
        | Table (lineNum, name, _, _) ->
            if (Map.isEmpty dimmed) = true || (Map.find ("%"+name) dimmed) = 1 then
                addTableIfNotExists name 11 11
        | _ -> ()
        
        for node in forStack do
            match node with
            | Variable(_,n) ->
                match unsubs with
                | Variable(_,n2) ->
                    if (n = n2) = true then
                        semanticError lineNum label "You can not nest FORs with the same variable."
                | _ -> ()
            | _ -> ()
        forStack <- unsubs :: forStack
        try
            List.iter (fun expr -> checkExpr expr) [exprStart]
            List.iter (fun expr -> checkExpr expr) [exprEnd]
            match exprStep with
            | Some(x) ->
                List.iter (fun expr -> checkExpr expr) [x]
            | None -> ()
        with
            | NotDefinedFunction(msg) -> semanticError lineNum label msg
            | IllegalNumberException(msg) -> semanticError lineNum label msg
        
    | Next (lineNum, label, unsubs) ->
        checkLabel lineNum label
        match unsubs with
        | Variable (lineNum, name) ->
            addScalarIfNotExists name
        | List (lineNum, name, _) ->
            if (Map.isEmpty dimmed) = true || (Map.find ("$"+name) dimmed) = 1 then
                ignore(addListIfNotExists name 11)
        | Table (lineNum, name, _, _) ->
            if (Map.isEmpty dimmed) = true || (Map.find ("%"+name) dimmed) = 1 then
                ignore(addTableIfNotExists name 11 11)
        | _ -> ()
        
        match forStack with
        | [] -> ()
        | hd :: tl ->
            match hd with
            | Variable(_,unsubFor) ->
                match unsubs with
                | Variable(_,name) ->
                    forStack <- tl
                    if (name = unsubFor) = false then
                        semanticError lineNum label (sprintf "Unmatched variables: FOR (var %s) with NEXT (var %s)." unsubFor name)
                | _ -> ()
            | _ -> ()
    | End (lineNum, label) ->
        checkLabel lineNum label
        hasEnd <- hasEnd + 1
    | Stop (lineNum, label) ->
        checkLabel lineNum label
    | Gosub (lineNum, label, gosubLine) ->
        checkLabel lineNum label
        try
            ignore(int(gosubLine))
        with
            _ -> semanticError lineNum label (sprintf "GOSUB line number '%s' is not an integer." gosubLine)
    | Return (lineNum, label) ->
        checkLabel lineNum label
    | Dim (lineNum, label,exprs) ->
        checkLabel lineNum label
        List.iter (fun e -> checkDim e) exprs
    | Rem (lineNum, label) ->
        checkLabel lineNum label
    | Def (lineNum, label, fnName, arg, expr) ->
        checkLabel lineNum label
        addFunctionIfNotExists fnName arg expr
        match arg with
        | Variable (lineNum, name) ->
            addScalarIfNotExists name
        | _ -> ()
        try
            List.iter (fun e -> checkExpr e) [expr]
        with
            | NotDefinedFunction(msg) -> semanticError lineNum label msg
            | IllegalNumberException(msg) -> semanticError lineNum label msg
               
let checkExistingLabels stmt =
    match stmt with
    | Goto (lineNum, label, gotoLine) ->
        let n = checkInt lineNum gotoLine
        if (Set.contains n labelsList) = false then
            semanticError lineNum label "GOTO must point to an existing label."
    | If (lineNum, label, expr1, relation, expr2 , thenLine) ->
        let n = checkInt lineNum thenLine
        if (Set.contains n labelsList) = false then
            semanticError lineNum label "IF-THEN must point to an existing label."
    | Gosub (lineNum, label, gosubLine) ->
        let n = checkInt lineNum gosubLine
        if (Set.contains n labelsList) = false then
            semanticError lineNum label "GOSUB must point to an existing label."
    | _ -> ()
         
let checkData stmt = 
    match stmt with
    | Data(lineNumber, label, _) ->
        isData <- true
    | _ -> ()
        
let checkLastStmt stmtList =
    match stmtList with
    | hd :: tl ->
        match hd with
        | End(lineNumber, label) -> ()
        | _ -> 
            failwithf "END must be the last statement."
    | _ -> ()
       
    
let semanticAnalysis parseTree =
    match parseTree with 
        | Program (stmtList) -> 
            checkLastStmt (List.rev stmtList)
            List.iter (fun stmt -> checkData stmt) stmtList
            List.iter (fun stmt -> analyze stmt) stmtList
            List.iter (fun stmt -> checkExistingLabels stmt) stmtList
            if (List.isEmpty forStack) = false then
                match forStack.[0] with
                | Variable(_,id) -> failwithf "FOR with variable %s does not have its NEXT." id
                | _ -> ()
            if hasEnd > 1 then
                failwithf "Only one END is permitted."
            scalarsTable
  
let getListsTable a =
    listsTable
let getTablesTable a =
    tablesTable
let getFunctionsTable a =
    functionsTable
