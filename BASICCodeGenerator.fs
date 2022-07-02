module BASICCodeGenerator

open BASICAST

let CILTypeScalar = "float64"
let CILTypeList = "float64[]"
let CILTypeTable = "float64[,]"

let mutable functions = Set.empty
let mutable returns = []
let mutable returnsList = []
let mutable forLabels = Map.empty
let mutable nextLabels = Map.empty
let mutable endLabels = Map.empty
let mutable stepLabels = Map.empty

let mutable counter = 0
let mutable gosub = 0

let mutable strResult = ""

let genDecls scalarsTable = 
     scalarsTable
        |> Set.toList
        |> List.map
            (fun scalar -> 
                sprintf 
                    "\t\t.locals init (%s '%s')\n"
                    CILTypeScalar
                    ("#"+scalar))
        |> String.concat ""
        
let genLists listsTable =
    listsTable
        |> Set.toList
        |> List.map 
            (fun (list,size) -> 
                sprintf 
                    "\t\t.locals init (%s '%s')\n\t\tldc.i4 %i\n\t\tnewarr [mscorlib]System.Double\n\t\tstloc '%s'\n"
                    CILTypeList
                    ("$"+list)
                    size
                    ("$"+list))
        |> String.concat ""
        
let genTables tablesTable =
    tablesTable
        |> Set.toList
        |> List.map
            (fun (table,size,size2) -> 
                sprintf 
                    "\t\t.locals init (%s '%s')\n\t\tldc.i4 %i\n\t\tldc.i4 %i\n\t\tnewobj instance void float64[,]::'.ctor'(int32,int32)\n\t\tstloc '%s'\n"
                    CILTypeTable
                    ("%"+table)
                    size size2
                    ("%"+table))
        |> String.concat ""   
            
let rec genBinOp op expr1 expr2 =
    sprintf "%s%s\t\t%s\n" (genExpr expr1) (genExpr expr2) op            
      
and genMathOp op expr =
    sprintf
        "%s\t\tcall float64 class ['basiclib']'Basic'.'Utils'::'%s'(float64)\n"
        (genExpr expr)
        op
        
and genPowOp op expr expr2 =
    sprintf "%s%s\t\tcall float64 class ['basiclib']'Basic'.'Utils'::'Pow'(float64,float64)\n" (genExpr expr) (genExpr expr2)
       
and genFn (Function (name,variable,expr)) fname fexpr farg =
    match variable with
    | Variable(line,vname) ->
        if fname = name then
            let argStr = match farg with
                         | Some(a) ->
                            (sprintf "\t\tldc.r8 %f\n" (float a))
                            + (sprintf "\t\tstloc '#%s'\n" vname)
                            + (genExpr expr)
                         | None -> "" 
            let exprStr = match fexpr with
                          | Some(e) ->
                            (genExpr e)
                            + (sprintf "\t\tstloc '#%s'\n" vname)
                            + (genExpr expr)
                          | None -> ""
            argStr + exprStr
        else 
            ""
    | _ -> ""

and genIntArg expr =
    match expr with
    
    | Variable (lineNum, varName) ->
        sprintf "\t\tldloc '%s'\n" ("#"+varName)
        
    | List (lineNum, varName, expr) ->
         sprintf "\t\tldloc '%s'\n" ("$"+varName)
         + (genIntArg expr)
         + "\t\tconv.i4 \n"
         + sprintf "\t\tldelem.r8\n"

    | Table (lineNum, varName, expr, expr2) ->
        sprintf "\t\tldloc '%s'\n" ("%"+varName)
        + (genIntArg expr)
        + "\t\tconv.i4 \n"
        + (genIntArg expr2)
        + "\t\tconv.i4 \n"
        + sprintf "\t\tcall instance float64 float64[,]::Get(int32, int32)\n"

    | Number (_, numVal) -> 
        sprintf "\t\tldc.i4 %i\n" (int numVal)
    
    | Fn (name, expr, arg) ->
        functions
        |> Set.map (fun elem -> genFn elem name expr arg)
        |> String.concat ""
        
    | Math (name, expr) ->
        genMathOp name expr
        
    | Plus (lineNum, expr, expr2) ->
        genBinOp "add" expr expr2
          
    | Minus(lineNum, expr, expr2) ->
        genBinOp "sub" expr expr2
        
    | Times(lineNum, expr, expr2) ->
        genBinOp "mul" expr expr2
        
    | Div(lineNum, expr, expr2) ->
        genBinOp "div" expr expr2
     
    | Pow(lineNum, expr, expr2) ->
        genPowOp "PowBas" expr expr2
        
    | _ -> ""
        
and genExpr expr =
    match expr with
    
    | Variable (lineNum, varName) ->
        sprintf "\t\tldloc '%s'\n" ("#"+varName)
        
    | List (lineNum, varName, expr) ->
         sprintf "\t\tldloc '%s'\n" ("$"+varName)
         + (genIntArg expr)
         + "\t\tconv.i4 \n"
         + sprintf "\t\tldelem.r8\n"

    | Table (lineNum, varName, expr, expr2) ->
        sprintf "\t\tldloc '%s'\n" ("%"+varName)
        + (genIntArg expr)
        + "\t\tconv.i4 \n"
        + (genIntArg expr2)
        + "\t\tconv.i4 \n"
        + sprintf "\t\tcall instance float64 float64[,]::Get(int32, int32)\n"

    | Number (_, numVal) -> 
        sprintf "\t\tldc.r8 %f\n" (float numVal)
    
    | Fn (name, expr, arg) ->
        functions
        |> Set.map (fun elem -> genFn elem name expr arg)
        |> String.concat ""
        
    | Math (name, expr) ->
        genMathOp name expr
        
    | Plus (lineNum, expr, expr2) ->
        genBinOp "add" expr expr2
          
    | Minus(lineNum, expr, expr2) ->
        genBinOp "sub" expr expr2
        
    | Times(lineNum, expr, expr2) ->
        genBinOp "mul" expr expr2
        
    | Div(lineNum, expr, expr2) ->
        genBinOp "div" expr expr2
     
    | Pow(lineNum, expr, expr2) ->
        genPowOp "PowBas" expr expr2
        
    | _ -> ""
    
let genPrint label elem =
    match elem with
        | PEmpty -> ""
        | PStr(msg) ->
            sprintf "\t\tldstr %s\n" msg
            + sprintf "\t\tcall void class [mscorlib]System.Console::Write(string)\n"
        | PExpr(expr) ->
            (genExpr expr)
            + sprintf "\t\tcall void class [mscorlib]System.Console::Write(float64)\n"
        | PComma ->
            sprintf "\t\tldstr \"%s\"\n" "\\t"
            + sprintf "\t\tcall void class [mscorlib]System.Console::Write(string)\n"
            
let genAssign label varName expr =
    ("AAS_"+label+":")
    + genExpr expr
    + sprintf "\t\tstloc '%s'\n" varName

let genAssignList label varName index expr =
    ("AAS_"+label+":")
    + sprintf "\t\tldloc '%s'\n" varName
    + (genIntArg index)
    + "\t\tconv.i4 \n"
    + (genExpr expr)
    + sprintf "\t\tstelem.r8\n"

let genAssignTable label varName index index2 expr =
    ("AAS_"+label+":") 
    + sprintf "\t\tldloc '%s'\n" varName
    + (genIntArg index)
    + "\t\tconv.i4 \n"
    + (genIntArg index2)
    + "\t\tconv.i4 \n"
    + (genExpr expr)
    + sprintf "\t\tcall instance void float64[,]::Set(int32, int32, float64)\n"
        
let genRel rel target =
    if rel = "=" then
        sprintf "\t\tbeq '%s'\n" ("AAS_"+target)
    elif rel = "<>" then
        sprintf "\t\tbne.un '%s'\n" ("AAS_"+target)
    elif rel = "<" then
        sprintf "\t\tblt '%s'\n" ("AAS_"+target)
    elif rel = ">" then
        sprintf "\t\tbgt '%s'\n" ("AAS_"+target)
    elif rel = "<=" then
        sprintf "\t\tble '%s'\n" ("AAS_"+target)
    elif rel = ">=" then
        sprintf "\t\tbge '%s'\n" ("AAS_"+target)
    else
        ""

let genRead variable =
    match variable with
    | Variable (_, varName) ->
        "\t\tcall float64 class ['basiclib']'Basic'.'Utils'::'Read'()\n"
        + sprintf "\t\tstloc '%s'\n" ("#"+varName)
    | List (_, varName, idx) ->
        sprintf "\t\tldloc '%s'\n" ("$"+varName)
        + (genIntArg idx)
        + "\t\tconv.i4 \n"
        + "\t\tcall float64 class ['basiclib']'Basic'.'Utils'::'Read'()\n"
        + sprintf "\t\tstelem.r8\n"
    | Table (_, varName, idx, idx2) ->
        sprintf "\t\tldloc '%s'\n" ("%"+varName)
        + (genIntArg idx)
        + "\t\tconv.i4 \n"
        + (genIntArg idx2)
        + "\t\tconv.i4 \n"
        + "\t\tcall float64 class ['basiclib']'Basic'.'Utils'::'Read'()\n"
        + sprintf "\t\tcall instance void float64[,]::Set(int32, int32, float64)\n"
    | _ -> ""
        
let genStmt stmt =
    match stmt with 
    | Let (_, label, variable, expr) -> 
        match variable with
            | Variable (_, varName) -> genAssign label ("#"+varName) expr
            | List (_, varName, index) -> genAssignList label ("$"+varName) index expr
            | Table (_, varName, index, index2) -> genAssignTable label ("%"+varName) index index2 expr
            | _ -> ""
    | Print(_,label, elemsPrint) ->
        if (List.head (List.rev elemsPrint)) = PComma then
            ("AAS_"+label+":")
            + (elemsPrint
            |> List.map (fun elem -> genPrint label elem)
            |> String.concat "")
        else
            ("AAS_"+label+":")
            + (elemsPrint
            |> List.map (fun elem -> genPrint label elem)
            |> String.concat "")
            + "\t\tcall void class [mscorlib]System.Console::WriteLine()\n"
    | Stop (_,label) ->
        ("AAS_"+label+":") 
        + sprintf "\t\tcall void class ['basiclib']'Basic'.'Utils'::'Exit'()\n"
    | Goto(_,label,gotoLine) ->
        ("AAS_"+label+":")
        + sprintf "\t\tbr '%s'\n" ("AAS_"+gotoLine)
    | End (_, label) ->
        ("AAS_"+label+":\n")
    | Rem (_, label) ->
        ("AAS_"+label+":\n")
    | If (_, label, expr1, rel, expr2, thenLine) ->
        ("AAS_"+label+":")
        + (genExpr expr1)
        + (genExpr expr2)
        + (genRel rel thenLine)
    | Read (_, label, exprs) ->
        ("AAS_"+label+":")
        + (exprs
        |> List.map (fun elem -> genRead elem)
        |> String.concat "")
    | For (_,label, Variable (lineNumber, varName), start, endFor, step) ->
        counter <- counter + 1
        endLabels <- Map.add (sprintf "endF%s" varName) (sprintf "endF%s%i" varName counter) endLabels
        stepLabels <- Map.add (sprintf "stepF%s" varName) (sprintf "stepF%s%i" varName counter) stepLabels
        forLabels <- Map.add (sprintf "AAS_F%s" varName) (sprintf "AAS_F%s%i" varName counter) forLabels
        nextLabels <- Map.add (sprintf "AAS_N%s" varName) (sprintf "AAS_N%s%i" varName counter) nextLabels
        match step with
        | None ->
            ("AAS_"+label+":")
            + sprintf "\t\t.locals init (float64 'endF%s%i')\n" varName counter
            + sprintf "\t\t.locals init (float64 'stepF%s%i')\n" varName counter
            + (genExpr endFor)
            + sprintf "\t\tstloc '%s'\n" (Map.find (sprintf "endF%s" varName) endLabels) 
            + "\t\tldc.r8 1\n"
            + sprintf "\t\tstloc '%s'\n" (Map.find (sprintf "stepF%s" varName) stepLabels) 
            + (genExpr start)
            + sprintf "\t\tstloc '%s'\n" ("#"+varName)
            + sprintf "\t\tbr '%s'\n" (Map.find (sprintf "AAS_N%s" varName) nextLabels) 
            + sprintf "%s:\n" (Map.find (sprintf "AAS_F%s"varName) forLabels) 
        | Some(s) ->
         ("AAS_"+label+":")
            + sprintf "\t\t.locals init (float64 'endF%s%i')\n" varName counter
            + sprintf "\t\t.locals init (float64 'stepF%s%i')\n" varName counter
            + (genExpr endFor)
            + sprintf "\t\tstloc '%s'\n" (Map.find (sprintf "endF%s" varName) endLabels) 
            + (genExpr s)
            + sprintf "\t\tstloc '%s'\n" (Map.find (sprintf "stepF%s" varName) stepLabels) 
            + (genExpr start)
            + sprintf "\t\tstloc '%s'\n" ("#"+varName)
            + sprintf "\t\tbr '%s'\n" (Map.find (sprintf "AAS_N%s" varName) nextLabels)
            + sprintf "%s:\n" (Map.find (sprintf "AAS_F%s"varName) forLabels) 
    | Next (_, label, Variable(lineNumber,varName)) ->
        ("AAS_"+label+":")
        + sprintf "\t\tldloc '%s'\n" ("#"+varName)
        + sprintf "\t\tldloc '%s'\n" (Map.find (sprintf "stepF%s" varName) stepLabels) 
        + "\t\tadd\n"
        + sprintf "\t\tstloc '%s'\n" ("#"+varName)
        + sprintf "%s:\n" (Map.find (sprintf "AAS_N%s" varName) nextLabels)
        + sprintf "\t\tldloc '%s'\n" ("#"+varName)
        + sprintf "\t\tldloc '%s'\n" (Map.find (sprintf "endF%s" varName) endLabels) 
        + sprintf "\t\tble '%s'\n" (Map.find (sprintf "AAS_F%s" varName) forLabels) 
        + sprintf "\t\tldloc '%s'\n" (Map.find (sprintf "endF%s" varName) endLabels) 
        + sprintf "\t\tstloc '%s'\n" ("#"+varName)
    | Gosub (_,label, gotoLabel) ->
        match returns with
        | [] -> ""
        | hd::tl -> returns <- tl
                    ("AAS_"+label+":")
                    + sprintf "\t\tldc.i4 %i\n\t\tcall void class ['basiclib']'Basic'.'Utils'::'Push'(int32)\n" hd
                    + sprintf "\t\tbr 'AAS_%i'\n" (int gotoLabel)
    | Return (_,label) -> 
        ("AAS_"+label+":")
        + "\t\tbr 'returnSwitch'\n"
    | _ -> ""
    
let genStmts stmtList =
    stmtList
    |> List.map genStmt
    |> String.concat ""
    
let genNumber number =
    sprintf "\t\tldc.r8 %f\n" (float number)
    + "\t\tcall void class ['basiclib']'Basic'.'Utils'::'Data'(float64)\n"
    
let genData stmt =
    match stmt with
    | Data(_,label, numbers) -> numbers
                                |> List.map genNumber
                                |> String.concat ""
    | _ -> ""
    
let genDatas stmtList =
    stmtList
    |> List.map genData
    |> String.concat ""
    
let getReturn stmt =
    match stmt with
    | Gosub (_,label, _) ->
        if gosub = 1 then
            gosub <- 1
            returns <- ((int label))::returns
            returnsList <- ((int label))::returnsList
        else 
            gosub <- 1
    | Let (_, label, _, _) ->
        if gosub = 1 then
            gosub <- 0
            returns <- ((int label))::returns
            returnsList <- ((int label))::returnsList
    | Print(_,label, _) ->
        if gosub = 1 then
            gosub <- 0
            returns <- ((int label))::returns
            returnsList <- ((int label))::returnsList
    | Stop (_,label) ->
        if gosub = 1 then
            gosub <- 0
            returns <- ((int label))::returns
            returnsList <- ((int label))::returnsList
    | Goto(_,label,_) ->
        if gosub = 1 then
            gosub <- 0
            returns <- ((int label))::returns
            returnsList <- ((int label))::returnsList
    | End (_, label) ->
        if gosub = 1 then
            gosub <- 0
            returns <- ((int label))::returns
            returnsList <- ((int label))::returnsList
    | Rem (_, label) ->
        if gosub = 1 then
            gosub <- 0
            returns <- ((int label))::returns
            returnsList <- ((int label))::returnsList
    | If (_, label, _, _, _, _) ->
        if gosub = 1 then
            gosub <- 0
            returns <- ((int label))::returns
            returnsList <- ((int label))::returnsList
    | Read (_, label, _) ->
         if gosub = 1 then
            gosub <- 0
            returns <- ((int label))::returns
            returnsList <- ((int label))::returnsList
    | For (_,label, _, _, _, _) ->
        if gosub = 1 then
            gosub <- 0
            returns <- ((int label))::returns
            returnsList <- ((int label))::returnsList
    | Next (_, label, _) ->
        if gosub = 1 then
            gosub <- 0
            returns <- ((int label))::returns
            returnsList <- ((int label))::returnsList
    | Return (_,label) -> 
        if gosub = 1 then
            gosub <- 0
            returns <- ((int label))::returns
            returnsList <- ((int label))::returnsList
    | Data(_,label,_) ->
        if gosub = 1 then
            gosub <- 0
            returns <- ((int label))::returns
            returnsList <- ((int label))::returnsList
    | Dim (_,label,_) ->
        if gosub = 1 then
            gosub <- 0
            returns <- ((int label))::returns
            returnsList <- ((int label))::returnsList
    | Def (_, label, _, _, _) ->
        if gosub = 1 then
            gosub <- 0
            returns <- ((int label))::returns
            returnsList <- ((int label))::returnsList

    
let getReturns stmtList =
    stmtList
    |> List.iter getReturn
    ""
    
let genReturnSwitch returnsL =
    strResult <- "\t\tcall int32 class ['basiclib']'Basic'.'Utils'::'Pop'()\n\t\tstloc 'tmp'\n"
    for item in returnsL do
        strResult <- strResult + sprintf "\t\tldloc 'tmp'\n\t\tldc.i4 %i\n\t\tbeq 'sub%i'\n" item item
    strResult
        
let genReturnSwitch2 returnsL =
    strResult <- ""
    for item in returnsL do
        strResult <- strResult + sprintf "sub%i:\t\tbr 'AAS_%i'\n" item item
    strResult
        
let generateCode scalarsT listsT tablesT functionsT parseTree =
    functions <- functionsT
    match parseTree with 
        | Program (stmtList) -> 
            let prologue =
                "// Code generated by the BASIC compiler.\n\n"
                + ".assembly 'basic' {}\n\n"
                + ".assembly extern 'basiclib' {}\n\n"
                + ".class public 'BASICProgram' extends "
                + "['mscorlib']'System'.'Object' {\n"
                + "\t.method public static void 'start'() {\n"
                + "\t\t.entrypoint\n"
                + "\t\t.maxstack 1024\n"
                + "\t\t.locals init (int32 'tmp')\n"
            let midprog = prologue 
                          + (genDecls scalarsT) 
                          + (genLists listsT) 
                          + (genTables tablesT)
                          + (genDatas stmtList)
                          + (getReturns stmtList)
            returns <- (List.rev returns)
            returnsList <- (List.rev returnsList)
            let midprog2 = (genStmts stmtList)
                           + "\t\tbr AAS_fin\n"
                           + "returnSwitch:\n"
                           + (genReturnSwitch returnsList)
                           + (genReturnSwitch2 returnsList)
                           + "AAS_fin:\t\tret\n"
                           + "\t}\n"
                           + "}"
            midprog + midprog2
