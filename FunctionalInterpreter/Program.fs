open System
open FParsec
open Parser
open EvaluatorOne
open System.IO

let printInfo() =
    Console.WriteLine "Write script below, press Shift+Enter to execute.\n"

let mergeTypes m1 m2 =
    let mutable outMap = m1
    for (key, type') in Map.toSeq m2 do
        if (not (Map.containsKey key outMap)) then
            outMap <- outMap.Add(key, type')
        else
            outMap <- outMap
    outMap

[<EntryPoint>]
let main argv = 
    if (argv.Length > 0) then
        let fileList = Array.toList argv
        for file in fileList do
            let result = runParserOnFile p_program UserState.Default file Text.Encoding.ASCII
            let outFile = file.Replace("\"", "") + ".log"
            let out = new StreamWriter(outFile, false)
            let mutable scopeList = [Scope.Default]
            match result with
            | Success(result, state,_) ->
                out.WriteLine(sprintf "Parse Output: \n%A\n" result);
                let scopes = List.reduce (fun s s1 -> { s with methods = s.methods @ s1.methods; types = mergeTypes s.types s1.types; } ) scopeList
                let evalRslt = evaluateProgram scopes result
                match evalRslt with
                | ISuccess(s, v) ->
                    scopeList <- scopeList @ [s]
                    out.WriteLine(sprintf "Program Output: \n%s\n" stdout)
                    out.WriteLine(sprintf "Final Scope: \n%A\nEvaluator Output: \n%A\n" s v)
                | IFailure(f) ->
                    out.WriteLine(sprintf "Evaluator Output: \n%s\n" f)
            | Failure(error,_,_) ->
                out.WriteLine("Parse Output: \n" + error);
            out.Close()
        0;
    else
        let mutable executeCode = false
        let mutable verbose = false
        let mutable continueLooping = true
        let mutable userInput = ""
        printInfo()
        while (continueLooping) do
            let keyInfo = Console.ReadKey()
            let shiftPressed = keyInfo.Modifiers.HasFlag(ConsoleModifiers.Shift)
            if (shiftPressed && keyInfo.Modifiers.HasFlag(ConsoleModifiers.Control) && keyInfo.Key = ConsoleKey.Enter) then
                executeCode <- true
                verbose <- true
            else if (shiftPressed && keyInfo.Key = ConsoleKey.Enter) then
                executeCode <- true
            else
                match keyInfo.Key with
                | ConsoleKey.Enter ->
                    userInput <- userInput + "\n"
                | ConsoleKey.Backspace ->
                    if (String.IsNullOrEmpty(userInput) <> true) then
                        userInput <- userInput.Remove(userInput.Length - 1)
                | c ->
                    userInput <- userInput + string(keyInfo.KeyChar)
                Console.Clear()
                printInfo()
                Console.Write(userInput)
            if (executeCode) then
                stdout <- ""
                Console.WriteLine()
                let result = (runParserOnString (many p_statement) UserState.Default "") userInput
                match result with
                | Success(rs, _, _) ->
                    if verbose then printfn "\nParse Success, AST: \n%A" rs else printf "Parse Success... "
                    printfn "Running output"
                    let intResult = evaluateProgram Scope.Default rs
                    match intResult with
                    | ISuccess (scope, value) ->
                        if verbose then printfn "Execution Success, final scope: \n%A" scope else printfn "Execution Success..."
                        printfn "Return Output: %A" value
                    | IFailure f ->
                        printfn "Execution Failure: %s" f
                | Failure(errorMessage, _, _) ->
                    printfn "\nParse Failure: %s" errorMessage
                printfn "Press any key to reset, or Escape to exit."

                let keyInfo = Console.ReadKey()
                if (keyInfo.Key <> ConsoleKey.Escape) then
                    executeCode <- false
                    continueLooping <- true
                    userInput <- ""
                    Console.Clear()
                    printInfo()
                else
                    continueLooping <- false
        0 // return an integer exit code