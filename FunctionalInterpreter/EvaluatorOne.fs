module EvaluatorOne

open FParsec
open System
open Parser
open AbstractSyntaxTree

// Internal representation of a definite value
type Value =
    | Null_ // Used internally
    | Int_ of int // Represents an integer
    | Float_ of Double // ..
    | Boolean_ of Boolean
    | String_ of String
    | Array_ of String * Value[] // Represents an array of typed values
    | Object_ of String * Map<String, Value> // Represents an object, just a string->value map

// Unboxes an int from internal representation to platform representation that can be operated on
let deint i =
    match i with
    | Int_ i1 -> i1

// Unboxes a float
let defloat f =
    match f with
    | Float_ f1 -> f1

// Unboxes a string
let destring s =
    match s with
    | String_ s1 -> s1

// Unboxes a boolean
let debool b =
    match b with
    | Boolean_ b1 -> b1

let dearray a = 
    match a with
    | Array_(type', a1) -> (type', a1)

let mutable stdout = ""

type Property = { pType: String; pName: String; pValue: Value; }

// Internal representation of a variable, with a name, type and value
type Variable = { name: string; varType: string; mutable value: Value; }
// Internal representation of a method, with a name, list of parameters, and the method body (statement list)
type Method = { position: Pos; parent: string; methodName: string; parameters: Parameter list; statements: Statement list; }
// Internal representation of a user-defined type, with a name, properties and (unused) associated methods
type UserType = { position: Pos; typeName: string; properties: Property list; methods: Method list }
// Internal representation of an in-built type, used to make the in-built types
type InbuiltType = { typeName: string; getters: (String * (Value -> Value)) list; }
// Internal representation of an in-built method
type InbuiltMethod = { name: string; parameters: Parameter list; mapping: (Value list -> Value) }

let writeMethod [x:_] =
    Console.WriteLine(destring(x))
    stdout <- String.Concat(stdout, (destring x))
    x

// The definitions for the in-built methods, the mapping property takes a function that accepts a list of Values, and returns a Value
// Methods to read and write from/to the Console
let write = { name = "write"; parameters = [ BasicParameter("value", "String") ]; mapping = writeMethod }
let writeLine = { name = "writeLine"; parameters = [ BasicParameter("value", "String") ]; mapping = fun [x] -> Console.WriteLine(destring(x)); stdout <- stdout + destring(x); x; }
let readLine = { name = "readLine"; parameters = []; mapping = fun x -> String_(Console.ReadLine()); }
// Methods that cast all the primitive types to strings
let intToString = { name = "String"; parameters = [ BasicParameter("value", "Int") ]; mapping = fun [x] -> String_(Convert.ToString(deint(x))); }
let floatToString = { name = "String"; parameters = [ BasicParameter("value", "Float") ]; mapping = fun [x] -> String_(Convert.ToString(defloat(x))); }
let booleanToString = { name = "String"; parameters = [ BasicParameter("value", "Boolean") ]; mapping = fun [x] -> String_(Convert.ToString(debool(x))); }
// Methods that cast from int to float, and float to int
let intToFloat = { name = "Float"; parameters = [ BasicParameter("value", "Int") ]; mapping = fun [x] -> Float_(Convert.ToDouble(deint(x))); }
let floatToInt = { name = "Int"; parameters = [ BasicParameter("value", "Float") ]; mapping = fun [x] -> Int_(Convert.ToInt32(defloat(x))); }

// Union type of either an In-Built method or User-Defined method
type AMethod =
    | Method' of Method
    | InbuiltMethod of InbuiltMethod

// Union type of either an In-Built type or User-Defined type.
type AType =
    | UserType of UserType
    | InbuiltType of InbuiltType

type ScopeType =
    | ScopeNone // The current scope is top-level
    | ScopeMethod // The scope is inside a method
    | ScopeType // Scope is inside a type declaration
    | ScopeBranch // Scope is inside a branch
    | ScopeDotNotation // Scope is handling dot notation

// Represents the state of the interpreter at one point of execution, stores all the variables, methods and types
type Scope = { isAssignment: Boolean; assignValue: Value; variables: Map<string, Variable>; 
    methods: AMethod list; types: Map<string, AType>; scopeType: ScopeType; position: Pos;}
                with static member Default = { isAssignment = false; assignValue = Null_; variables = Map.empty; 
                                                methods = [ InbuiltMethod(write);
                                                            InbuiltMethod(writeLine);
                                                            InbuiltMethod(readLine);
                                                            InbuiltMethod(intToString);
                                                            InbuiltMethod(floatToString);
                                                            InbuiltMethod(booleanToString);
                                                            InbuiltMethod(intToFloat);
                                                            InbuiltMethod(floatToInt);
                                                            ];
                                                types = [   "Int", InbuiltType({ typeName = "Int"; getters = List.empty; });
                                                            "Float", InbuiltType({ typeName = "Float"; getters = List.empty; });
                                                            "String", InbuiltType({ typeName = "String"; getters = ["length", fun s -> Int_(destring(s).Length);
                                                                                                                    "empty", fun s -> Boolean_(destring(s).Length = 0);

                                                            ]})
                                                            "Boolean", InbuiltType({ typeName = "Boolean"; getters = List.empty; })
                                                            "Array", InbuiltType({ typeName = "Array"; getters = ["length", fun a -> Int_((snd (dearray a)).Length);] }) 
                                                        ] |> Map.ofList;
                                                scopeType = ScopeType.ScopeNone; position = (int64(0), int64(0)); }

// Union type. Stores the result of an evaluated statement or expression, or a fail message if an error occurs
type Result<'a> =
    | ISuccess of 'a
    | IFailure of string

// Helper function to create an error message with a position and message
let createError pos message =
    let (line, col) = pos
    sprintf "[%i, %i] %s" line col message

// Helper function, converts a Value's type into a string representation
let valName v = match v with
                | Null_ -> "Null"
                | Int_ _ -> "Int"
                | Float_ _ -> "Float"
                | Boolean_ _ -> "Boolean"
                | String_ _ -> "String"
                | Array_ (t, _) -> sprintf "[%s]" t
                | Object_ (s, v) -> s

let isArray v = match v with
                | Array_(_,_) -> true
                | _ -> false

// Helper function, converts an operator into a string representation
let opName o = match o with
                | Add -> "add"
                | Sub -> "subtract"
                | Div -> "divide"
                | Mul -> "multiply"
                | Mod -> "modulo"

let typeError pos t1 t2 =
    createError pos (sprintf "Type error: %s and %s are not the same type."  t1 t2)

// Helper function, checks if a list of pairs contain the same type
let sameTypes lsVal =
    List.fold (fun val1 val2 -> val1.GetType() = val2.GetType()) (true) lsVal

// Helper functions for reporting operation errors
let opError pos op t1 t2 =
    createError pos (sprintf "Operation error: %s cannot be applied to %s and %s." (opName op) t1 t2)

let opError' pos op t1 =
    createError pos (sprintf "Operation error: %s cannot be applied to %s." (opName op) t1)

// Helper function that takes a type, type -> method map for use by operators during execution
// If the two types are in the map, it returns the method to use for performing said operator on those types
let typeMapGet pos (m: Map<string * string, (Value -> Value -> Value)>) op l r =
    if (not (m.ContainsKey (l, r))) then 
        IFailure(opError pos op l r)
    else
        ISuccess(m.[(l, r)])

// Performs an addition on the in-built types
let add pos left right =
    // Define the (type, type) -> method map
    let map = [   ("Int", "Int"),           (fun x y -> Int_(deint x + deint y));
                  ("Int", "Float"),         (fun x y -> Float_(float(deint x) + defloat y));
                  ("Float", "Int"),         (fun x y -> Float_(defloat x + float(deint y)));
                  ("Float", "Float"),       (fun x y -> Float_(defloat x + defloat y));
                  ("String", "String"),     (fun x y -> String_((destring x) + (destring y)));
                  ("String", "Int"),        (fun x y -> String_((destring x) + (Convert.ToString(deint y))));
                  ("String", "Float"),      (fun x y -> String_((destring x) + (Convert.ToString(defloat y))));
                  ("String", "Boolean"),    (fun x y -> String_((destring x) + (Convert.ToString(debool y)))); ]
                |> Map.ofList
    // Return the appropriate method for the types passed in
    typeMapGet pos map Add left right

// Performs subtraction on in-built types
let sub pos left right =
    let map = [   ("Int", "Int"),           (fun x y -> Int_(deint x - deint y));
                  ("Int", "Float"),         (fun x y -> Float_(float(deint x) - defloat y));
                  ("Float", "Int"),         (fun x y -> Float_(defloat x - float(deint y)));
                  ("Float", "Float"),       (fun x y -> Float_(defloat x - defloat y)); ]
                |> Map.ofList
    typeMapGet pos map Sub left right

// Performs multiplication on in-built types
let mul pos left right =
    let map = [   ("Int", "Int"),           (fun x y -> Int_(deint x * deint y));
                  ("Int", "Float"),         (fun x y -> Float_(float(deint x) * defloat y));
                  ("Float", "Int"),         (fun x y -> Float_(defloat x * float(deint y)));
                  ("Float", "Float"),       (fun x y -> Float_(defloat x * defloat y)); 
                  ("String", "Int"),        (fun x y -> String_(String.replicate (deint y) (destring x))); ]
                |> Map.ofList
    typeMapGet pos map Mul left right

// Performs division on in-built types
let div pos left right =
    let map = [   ("Int", "Int"),           (fun x y -> Int_(deint x / deint y));
                  ("Int", "Float"),         (fun x y -> Float_(float(deint x) / defloat y));
                  ("Float", "Int"),         (fun x y -> Float_(defloat x / float(deint y)));
                  ("Float", "Float"),       (fun x y -> Float_(defloat x / defloat y)); ]
                |> Map.ofList
    typeMapGet pos map Div left right

// Performs a modulo operation on integers
let mod' pos left right =
    let map = [   ("Int", "Int"),           (fun x y -> Int_(deint x % deint y)); ]
                |> Map.ofList
    typeMapGet pos map Mod left right

// Performs a logical and on two boolean values
let and' pos left right =
    let map = [   ("Boolean", "Boolean"),   (fun x y -> Boolean_(debool x && debool y)); ]
                |> Map.ofList
    typeMapGet pos map And left right

// Performs a logical or operation
let or' pos left right =
    let map = [   ("Boolean", "Boolean"),   (fun x y -> Boolean_(debool x || debool y)); ]
                |> Map.ofList
    typeMapGet pos map Or left right

// Performs an equality check on in-built types
let eq pos left right =
    let map = [   ("Int", "Int"),           (fun x y -> Boolean_(deint x = deint y));
                  ("Int", "Float"),         (fun x y -> Boolean_(float(deint x) = defloat y));
                  ("Float", "Int"),         (fun x y -> Boolean_(defloat x = float(deint y)));
                  ("Float", "Float"),       (fun x y -> Boolean_(defloat x = defloat y));
                  ("String", "String"),     (fun x y -> Boolean_((destring x) = (destring y))); 
                  ("Boolean", "Boolean"),   (fun x y -> Boolean_(debool x = debool y)); ]
                |> Map.ofList
    typeMapGet pos map Equality left right

// Performs an inequality check on inbuilt types
let neq pos left right = 
    let map = [   ("Int", "Int"),           (fun x y -> Boolean_(deint x <> deint y));
                  ("Int", "Float"),         (fun x y -> Boolean_(float(deint x) <> defloat y));
                  ("Float", "Int"),         (fun x y -> Boolean_(defloat x <> float(deint y)));
                  ("Float", "Float"),       (fun x y -> Boolean_(defloat x <> defloat y));
                  ("String", "String"),     (fun x y -> Boolean_((destring x) <> (destring y))); 
                  ("Boolean", "Boolean"),   (fun x y -> Boolean_(debool x <> debool y)); ]
                |> Map.ofList
    typeMapGet pos map Inequality left right

// Performs a pow (x^n) on in-built types
let pow pos left right =
    let map = [   ("Int", "Int"),           (fun x y -> Int_(Convert.ToInt32(Math.Pow(Convert.ToDouble(deint x),  Convert.ToDouble(deint y)))));
                  ("Int", "Float"),         (fun x y -> Float_(Math.Pow(Convert.ToDouble(deint x), defloat y)));
                  ("Float", "Int"),         (fun x y -> Float_(Math.Pow(defloat x, Convert.ToDouble(deint y))));
                  ("Float", "Float"),       (fun x y -> Float_(Math.Pow(defloat x, defloat y))); ]
                |> Map.ofList
    typeMapGet pos map Pow left right

// Performs a greater-than check on in-built types, returns a boolean
let grthan pos left right =
    let map = [   ("Int", "Int"),           (fun x y -> Boolean_(deint x > deint y));
                  ("Int", "Float"),         (fun x y -> Boolean_(float(deint x) > defloat y));
                  ("Float", "Int"),         (fun x y -> Boolean_(defloat x > float(deint y)));
                  ("Float", "Float"),       (fun x y -> Boolean_(defloat x > defloat y)); ]
                |> Map.ofList
    typeMapGet pos map GrThan left right

// Performs a less-than check on in-built types, returns a boolean
let lsthan pos left right =
    let map = [   ("Int", "Int"),           (fun x y -> Boolean_(deint x < deint y));
                  ("Int", "Float"),         (fun x y -> Boolean_(float(deint x) < defloat y));
                  ("Float", "Int"),         (fun x y -> Boolean_(defloat x < float(deint y)));
                  ("Float", "Float"),       (fun x y -> Boolean_(defloat x < defloat y)); ]
                |> Map.ofList
    typeMapGet pos map LsThan left right

// Performs a greater-than or equal to check on in-built types
let grthaneq pos left right =
    let map = [   ("Int", "Int"),           (fun x y -> Boolean_(deint x >= deint y));
                  ("Int", "Float"),         (fun x y -> Boolean_(float(deint x) >= defloat y));
                  ("Float", "Int"),         (fun x y -> Boolean_(defloat x >= float(deint y)));
                  ("Float", "Float"),       (fun x y -> Boolean_(defloat x >= defloat y)); ]
                |> Map.ofList
    typeMapGet pos map GrThanEq left right

// Performs a less-than or equal to check on in-built types
let lsthaneq pos left right =
    let map = [   ("Int", "Int"),           (fun x y -> Boolean_(deint x <= deint y));
                  ("Int", "Float"),         (fun x y -> Boolean_(float(deint x) <= defloat y));
                  ("Float", "Int"),         (fun x y -> Boolean_(defloat x <= float(deint y)));
                  ("Float", "Float"),       (fun x y -> Boolean_(defloat x <= defloat y)); ]
                |> Map.ofList
    typeMapGet pos map LsThanEq left right

// Helper function that returns the result of applying 'op' to left and right
let rec handleOp scope op left right =
    // Get the type of the left value
    let ltype = valName left
    // Get the type of the right value
    let rtype = valName right
    // Get the general position from the scope
    let p = scope.position
    // Match the operator against the supported operators, and return the appropriate helper function
    // to handle the operation
    match op with
    | Add       -> add p ltype rtype
    | Sub       -> sub p ltype rtype
    | Mul       -> mul p ltype rtype
    | Div       -> div p ltype rtype
    | Mod       -> mod' p ltype rtype
    | And       -> and' p ltype rtype
    | Or        -> or' p ltype rtype
    | Pow       -> pow p ltype rtype
    | Equality  -> eq p ltype rtype
    | Inequality-> neq p ltype rtype
    | GrThan    -> grthan p ltype rtype
    | LsThan    -> lsthan p ltype rtype
    | GrThanEq  -> grthaneq p ltype rtype
    | LsThanEq  -> lsthaneq p ltype rtype

// Evaluates an infix operator (operator with a left and right operand)
let rec evaluateInfixOp scope (lhs: (Scope * Value)) (rhs: (Scope * Value)) op =
    // Get the left hand side
    let (_, lhs) = lhs
    // Get the right hand side
    let (_, rhs) = rhs
    // Check if left item is an array
    match lhs with
    | Array_(tname1, a1) ->
        match rhs with
        | Array_(tname2, a2) ->
            if (tname1 = tname2) then
                // Concatenate the two arrays
                ISuccess(scope, Array_(tname1, Array.concat ([a1; a2])))
            else
                IFailure(createError scope.position (sprintf "Cannot concatenate arrays of type %s and type %s." tname1 tname2))
        | x ->
            // Add the item to the array if it's type matches
            if (valName x = tname1) then
                ISuccess(scope, Array_(tname1, Array.concat ([a1; [|x|]])))
            else
                IFailure(createError scope.position (sprintf "Cannot add item of type %s to array of type %s." (valName x) tname1))
    | lhs ->
        match rhs with
        | Array_(tname1, a1) ->
            if (valName lhs = tname1) then
                ISuccess(scope, Array_(tname1, Array.concat ([[|lhs|]; a1])))
            else
                IFailure(createError scope.position (sprintf "Cannot add item of type %s to array of type %s." (valName lhs) tname1))
        | rhs ->
            // Get the helper method appropriate for the types of the left and right operands
            let r = handleOp scope op lhs rhs
            match r with
            | ISuccess (s) ->
                // If a helper function for the two types exists, apply it to the left and right operands and return successfully
                ISuccess(scope, s lhs rhs)
            | IFailure (f) ->
                // If no helper function exists, fail with the pre-made message
                IFailure (f)

// The same as evaluateInfixOp, handles boolean not operations (!true) and
// unary minus/positive expressions on integers and floats
let evaluatePrefixOp scope rhs op =
    let (_, rhs) = rhs
    match op with
    | Not ->
        match rhs with
        | Boolean_ b ->
            ISuccess (scope, Boolean_(not b))
        | _ ->
            IFailure (opError' scope.position op (valName rhs))
    | Add ->
        match rhs with
        | Int_ i ->
            ISuccess(scope, Int_(+i))
        | Float_ f ->
            ISuccess(scope, Float_(+f))
        | _ ->
            IFailure (opError' scope.position op (valName rhs))
    | Sub ->
        match rhs with
        | Int_ i ->
            ISuccess (scope, Int_(-i))
        | Float_ f ->
            ISuccess (scope, Float_(-f))
        | _ ->
            IFailure (opError' scope.position op (valName rhs))
    | _ ->
        IFailure (opError' scope.position op (valName rhs))

// Checks if a method mname has been defined
let methodExists scope mname =
    let methodName = function | Method'(m) -> m.methodName
                              | InbuiltMethod(m) -> m.name
    List.fold (fun exists mthod -> exists || (methodName mthod) = mname) false scope.methods

// Checks if a type oname has been defined
let typeExists scope oname =
    if (scope.types.ContainsKey(oname)) then
        true
    else if (oname.StartsWith("[")) then
        // If the type name starts with a '[', it's an array type
        let arrayType = oname.Substring(1, oname.Length - 2)
        // If the type of the array has been defined, then so has the array type
        if (scope.types.ContainsKey(arrayType)) then
            true
        else
            false
    else
        false

// Checks if a type tname has been defined *by the user*
let userTypeExists scope tname =
    if (typeExists scope (tname)) then
        if (tname.StartsWith("[")) then
            false
        else
            let t = scope.types.[tname]
            match t with
            | UserType(_) ->
                true
            | InbuiltType(_) ->
                false
    else
        false

// Checks if a variable has been defined in scope
let variableExists scope vname =
    if (scope.variables.ContainsKey(vname)) then true else false

let handleArguments scope mname (params': Parameter List) (args: Result<Scope * Value> List) =
    // Check parameters and arguments lists have the same length
    if (params'.Length = args.Length) then
        // Get a list of the required types of each parameter
        let paramTypes = List.map (fun x -> match x with | BasicParameter(name, type') -> type') params'
        // Check to see if the types of the parameters are actual types (exist in the scope)
        let typeCorrectParams = List.forall (fun t -> typeExists scope t) paramTypes
        if (typeCorrectParams) then
            // Validate that the arguments were all evaluated successfully
            if (List.forall (fun x -> match x with | ISuccess(_, _) -> true | IFailure(_) -> false) args) then
                // Validate that each parameter and argument possess the same type
                let paramArgs = List.zip paramTypes (List.map (function | ISuccess(s,v) -> v) args)
                let firstMismatch = List.tryFind (fun (type', value) -> type' <> valName value) paramArgs
                match firstMismatch with
                | Some (type', value) ->
                    IFailure (createError scope.position (sprintf "Expected type %s, found type %s in arguments for %s" type' (valName value) mname))
                | None ->
                    // Get a list of the names of the parameters
                    let pnames = List.map (fun x -> match x with | BasicParameter(n, t) -> n) params'
                    // Combine the parameter name and argument lists
                    let paramArgs = List.zip pnames args
                    // Map each parameter, argument value pair into a variable structure
                    let newVariables = List.map (fun (pname, value) -> match value with | ISuccess (s, v) -> { name = pname; varType = valName v; value = v; }) paramArgs
                    // For each new variable, add it to a new scope and return the new scope
                    let mutable newScope = scope
                    for var in newVariables do
                        newScope <- { newScope with variables = newScope.variables.Add(var.name, var) }
                    ISuccess (newScope)
            else
                // If an argument didn't evaluate successfully, return that error
                let err = List.find (fun v -> match v with | IFailure _ -> true | _ -> false) args
                match err with
                | IFailure f ->
                    IFailure f
        else
            // If one of the parameters is of an unknown type, fail with an error
            let type' = List.find (fun t -> not (typeExists scope t)) paramTypes
            IFailure (createError scope.position (sprintf "Type %s does not exist." type'))
    else
        // If the parameters and arguments lists are not the same size, return with an error
        IFailure (createError scope.position (sprintf "Invalid number of arguments supplied to %s." mname))

// Helper method that returns a 'method' structure with the given parameters
let declareMethod pos name params' statements =
    { position = pos; parent = ""; methodName = name; parameters = params'; statements = statements; }

// Helper method that returns a 'type' structure with the given name and properties
let declareType pos name properties =
    { position = pos; typeName = name; properties = properties; methods = List.empty; }

// Generates a constructor method for a type when it is defined
let generateConstructor scope type' =
    // Pretend the constructor is within the type definition
    let pos = scope.position
    // The type will always be a user type, an already defined type such as an in-built type will have caused an error
    match type' with
    | UserType(u) ->
        // Get the (name, type) string tuples from the type
        let paramStrings =  [  for property in u.properties do
                                    yield (property.pName, property.pType)
                             ]
        // Convert the paramStrings list into a list of BasicParameters
        let params' = List.map (fun (x, y) -> BasicParameter(x, y)) paramStrings
        let name = u.typeName
        // Generated statement that creates a new empty instance of the type
        let init = Expr(pos, UntypedVarDecl("obj_" + u.typeName, FunctionCall(u.typeName, List.empty)))
        // Generate a list of statements assigning each property to a one of the parameters
        let statements = init :: [  for param in paramStrings do
                                        yield Assignment(pos, InfixOp(Identifier("obj_" + u.typeName), Dot, Identifier(fst param)), Identifier(fst param))
                                 ]
        // Return the constructor method
        declareMethod pos name params' statements

// Returns the default value for a type
let defaultValue vType =
    match vType with
    | "Null" -> Null_
    | "Int" -> Int_(0)
    | "Float" -> Float_(0.0)
    | "Boolean" -> Boolean_(false)
    | "String" -> String_("")
    | type' ->
        if (type'.StartsWith("[")) then
            let type' = type'.Substring(1, type'.Length - 2)
            Array_(type', Array.empty)
        else
            Object_(type', Map.empty)
            
// Handles locating a method overload with argTypes
let handleOverloads scope mname argTypes =
    // Make sure a method with mname actually exists
    if (methodExists scope mname) then
        // Obtain a list of all the methods with the name, mname
        let methods = List.filter (function | Method'(m) -> m.methodName = mname
                                            | InbuiltMethod(m) -> m.name = mname) scope.methods
        // Overload matching method
        let getOverload method' =
            match method' with
            // Check if the method is a user-defined or in-built one
            | Method'(m) ->
                // Get a list of the types of the parameters accepted by this method
                let typeList = List.map (function | BasicParameter(name, type') -> type') m.parameters
                // Combine the list of param types, and the list of argument types
                let zipTypes = List.zip typeList argTypes
                // Attempt to find a mismatch
                let firstMismatch = List.tryFind (fun (expectedType, givenType) -> expectedType <> givenType) zipTypes
                match firstMismatch with
                | Some(_) ->
                    // If there is a mismatch, this method obviously isn't the right one
                    let argStr = List.fold (fun str type' -> if (str = "") then type' else str + "," + type') "" argTypes
                    IFailure(createError scope.position (sprintf "Could not find overload of method %s, with parameter types (%s)." mname argStr))
                | None ->
                    // No mismatch, this methods param types and arg types match!
                    ISuccess(scope, method')
            | InbuiltMethod(m) ->
                // Performs the same operation as above, just on in-built methods
                let typeList = List.map (function | BasicParameter(name, type') -> type') m.parameters
                let zipTypes = List.zip typeList argTypes
                let firstMismatch = List.tryFind (fun (expectedType, givenType) -> expectedType <> givenType) zipTypes
                match firstMismatch with
                | Some(_) ->
                    let argStr = List.fold (fun str type' -> if (str = "") then type' else ", " + type') "" argTypes
                    IFailure(createError scope.position (sprintf "Could not find overload of method %s, with parameter types (%s)." mname argStr))
                | None ->
                    ISuccess(scope, method')
        // Attempt to find an overload from the list of methods
        let successfulOverload = List.tryFind (fun m -> match getOverload m with | ISuccess(_,_) -> true | IFailure(_) -> false) methods
        match successfulOverload with
        | Some (overload) ->
            // An overload was found, return it
            ISuccess (scope, overload)
        | None ->
            // No overload was found, so just get the first error we can find
            let error = List.find (fun m -> match getOverload m with | IFailure(f) -> true | ISuccess(_,_) -> false) methods
            match getOverload error with
            | IFailure(f) ->
                IFailure(f)
    else
        // The method does not exist!
        IFailure(createError scope.position (sprintf "Method %s does not exist" mname))

// Takes a new scope, an old scope, and merges the changes to any variables that existed before
// the new scope, discards new variables
let revertScopeKeepChanges result oldScope =
    match result with
    | ISuccess(s, result) ->
        let newScopeVars = s.variables
        let oldScopeVars = oldScope.variables
        let changedVars = Map.filter (fun vName var -> oldScopeVars.ContainsKey(vName)) newScopeVars
        let newScope = { oldScope with variables = changedVars }
        ISuccess(newScope, result)
    | IFailure(f) ->
        IFailure(f)

// Evaluates a statement, takes the current scope and a statement
// Returns ISuccess or IFailure
let rec evaluateStatement scope statement =
    match statement with
    // Empty statement
    | EmptyStatement (pos) ->
        let newScope = { scope with position = pos }
        ISuccess (newScope, Null_)
    // Expression, run expression evaluator on it
    | Expr (pos, expr) ->
        let newScope = { scope with position = pos }
        evaluateExpression newScope expr
    // Statement is a method declaration
    | Method (pos, (mname, plist, slist)) ->
        let scope = { scope with position = pos }
        // Make sure a type isn't already declared with the same name
        if (typeExists scope mname) then
            IFailure (createError scope.position (sprintf "%s already exists as a Type." mname))
        else if (methodExists scope mname) then
            // Method already exists, check to see if an overload with the same args already exists too
            let paramTypes = List.map (function | BasicParameter(n, t) -> t) plist
            let overloads = handleOverloads scope mname paramTypes
            match overloads with
            | IFailure(_) ->
                // No overload with the same args found, so this method can be added to the scope
                let scope = { scope with methods = scope.methods @ [Method'(declareMethod pos mname plist slist)]; }
                ISuccess (scope, Null_)
            | ISuccess(_,_) ->
                // Method with same name and same param types found, return an error
                let argStr = List.fold (fun str type' -> if (str = "") then type' else ", " + type') "" paramTypes
                IFailure (createError scope.position (sprintf "Method %s with argument types (%s) is already defined." mname argStr))
        else
            // Method with name doesn't exist, so this method is added
            let scope = { scope with methods = scope.methods @ [Method'(declareMethod pos mname plist slist)]; }
            ISuccess (scope, Null_)
    // Statement is a type declaration
    | Type (pos, (tname, properties)) ->
        // Update scope with the position of this type statement
        let scope = { scope with position = pos }
        // Check to make sure neither a method or type have already been defined that doesn't have the same name
        if (typeExists scope tname || methodExists scope tname) then
            IFailure (createError scope.position (sprintf "Type %s is already defined." tname))
        else
            // Convert the list of variables to a property list
            let properties = generatePropertyList scope properties
            match properties with
            | ISuccess(s, properties) ->
                // Properties converted successfully, declare the type
                let type' = UserType(declareType pos tname properties)
                // Create an auto-generated constructor
                let constructor' = generateConstructor scope type'
                // Add new type and its constructor to the scope
                let scope = { scope with types = scope.types.Add(tname, type'); methods = scope.methods @ [Method'(constructor')]; }
                ISuccess (scope, Null_)
            | IFailure(f) ->
                // Properties are not valid, error
                IFailure(f)
    // Assignment statement
    | Assignment (pos, left, right) ->
        let mutable scope = { scope with position = pos; }
        // Get the right-hand value
        let rightVal = evaluateExpression scope right
        match rightVal with
        | ISuccess(s, r) ->
            // Update the scope with info on the assignment
            scope <- { scope with assignValue = r; isAssignment = true; }
            match left with
            // The left hand side is an identifier
            | Identifier(l) ->
                // Check the left hand side exists as a variable
                if (variableExists scope l) then
                    if (scope.variables.[l].varType = valName r) then
                        // If the variable exists, assign it a new value
                        scope.variables.[l].value <- scope.assignValue
                        // Reset the scope to have to assignment
                        ISuccess({ scope with assignValue = Null_; isAssignment = false; }, scope.assignValue)
                    else
                        IFailure(createError scope.position (sprintf "Cannot assign value of type %s to variable of type %s." (valName r) (scope.variables.[l].varType)))
                else
                    IFailure(createError scope.position (sprintf "Variable %s does not exist." l))
            // The left hand side is a dot notation expression
            | InfixOp(lhs, Dot, rhs) ->
                match lhs with
                | Identifier(l) ->
                    // Evaluate the left hand expression to get a parent
                    let leftValue = evaluateExpression scope lhs
                    match leftValue with
                    | ISuccess(scope, leftValue) ->
                        // Call evaluateDotNotation to handle further expressions
                        let result = evaluateDotNotation scope leftValue rhs
                        match result with
                        | ISuccess(scope, mutatedObject) ->
                            // If dot notation was successful, update the left hand side to contain the new
                            // value with the assignment and return success
                            let scope = { scope with assignValue = Null_; isAssignment = false; }
                            scope.variables.[l].value <- mutatedObject
                            ISuccess(scope, mutatedObject)
                        | IFailure(f) ->
                            IFailure(f)
                    | IFailure(f) ->
                        IFailure(f)
                | _ ->
                    IFailure(createError scope.position (sprintf "Dot notation can only be used on object variables."))
            // ArrayGet in an assignment, setting an array item
            | ArrayGet(a, idx) ->
                // Get the array
                let array = evaluateExpression scope a
                // Evaluate the index
                let index = evaluateExpression scope idx
                match index with
                | ISuccess (s, i) ->
                    match i with
                    | Int_ (idx) ->
                        // The index is evaluated successfully and is an integer
                        match array with
                        | ISuccess (s, v) ->
                            match v with
                            | Array_(type', ls) ->
                                // The array is in fact an array, and evaluated successfully
                                if (valName scope.assignValue = type') then
                                    // Make sure the index is within the bounds of the array
                                    if (ls.Length <= idx) then
                                        IFailure (createError s.position "Index was outside bounds of array.")
                                    else if (idx < 0) then
                                        IFailure (createError s.position "Index cannot be less than zero.")
                                    else
                                        // Set the array item at the index to the assign value
                                        Array.set ls idx scope.assignValue
                                        let scope = { s with assignValue = Null_; isAssignment = false; }
                                        // Return successfully
                                        ISuccess (scope, Array_(type', ls))
                                else
                                    IFailure(createError s.position (sprintf "Cannot put type %s in %s Array." (valName scope.assignValue) type'))
                            | _ ->
                                IFailure (createError s.position (sprintf "%s is not an array." (valName v)))
                        | IFailure f ->
                            IFailure f
                    | _ -> 
                        IFailure (createError s.position "Array index must be an integer.")
                | IFailure f ->
                    IFailure f
            | _ ->
                IFailure(createError scope.position (sprintf "Assignment can only be used on variables or object properties."))
        | IFailure(f) ->
            IFailure f
    // Evaluating an If statement
    | IfStatement (pos, (condition, body)) ->
        // Update scope...
        let scope = { scope with position = pos }
        // Evaluate the condition of the if statement
        let conditionResult = evaluateExpression scope condition
        match conditionResult with
        // Check to see if the condition evaluated successfully
        | ISuccess(scope', value) ->
            match value with
            // Make sure the condition was a boolean expression, if it wasn't, fail
            | Boolean_(b) ->
                if (b = true) then
                    // If the condition was true, execute the code block stored in the if statement
                    revertScopeKeepChanges (handleStatementList scope' body) scope
                else
                    // Otherwise the If statement failed, return null
                    ISuccess(scope, Null_)
            | _ ->
                IFailure(createError scope.position "If statement condition must evaluate to a Boolean value.")
        | IFailure(f) ->
            IFailure(f)
    // If-Else Statement is being evaluated
    | IfElseStatement (pos, (ifStatement, elseBody)) ->
        let scope = { scope with position = pos; }
        // Evaluate the If statement
        let ifStatementResult = revertScopeKeepChanges (evaluateStatement scope ifStatement) scope
        match ifStatementResult with
        | ISuccess(scope', value) ->
            match value with
            // If the if statement returns null, then it failed, so try the else statement
            | Null_ ->
                revertScopeKeepChanges (handleStatementList scope' elseBody) scope
            | _ ->
                // If statement didn't return null, so skip the else statement
                ifStatementResult
        | IFailure(f) ->
            IFailure(f)
    // If-Else-If statement chain is being evaluated
    | IfElseIfStatement (ifStatement1, ifStatements) ->
        // Evaluate the top level if statement
        let ifStatement1Result = revertScopeKeepChanges (evaluateStatement scope ifStatement1) scope
        match ifStatement1Result with
        | ISuccess(scope', value) ->
            match value with
            | Null_ ->
                // The top level if statement failed, so move on to the next else or else-if statement in the chain
                revertScopeKeepChanges (evaluateStatement scope' ifStatements) scope
            | _ ->
                // Top level if statement succeeded, return
                ifStatement1Result
        | IFailure(f) ->
            IFailure(f)
    // For Statement is being evaluated
    | ForStatement (pos, (initialiser, condition, iterator, body)) ->
        let scope = { scope with position = pos }
        // Evaluate the initialiser
        match evaluateExpression scope initialiser with
        | ISuccess(scope', initialValue) ->
            match initialValue with
            // The initial value was an int
            | Int_(i) ->
                // Call handleForLoop to recursively execute the code block until the condition fails
                let result = handleForLoop scope' condition iterator body
                match result with
                | ISuccess(scope', value) ->
                    // Return the value of the code block
                    ISuccess(scope, value)
                | IFailure(f) ->
                    IFailure(f)
            // Initial value was a float
            | Float_(f) ->
                let result = handleForLoop scope' condition iterator body
                match result with
                | ISuccess(scope', value) ->
                    ISuccess(scope, value)
                | IFailure(f) ->
                    IFailure(f)
            // Initial value was not a valid value for a for statement
            | _ ->
                IFailure(createError scope.position "For loop iterator must be a number.")
        | IFailure(f) ->
            IFailure(f)
    // Assertion statement is being evaluated
    | Assertion (pos, expression) ->
        let scope = { scope with position = pos }
        // Evaluate the expression to be asserted
        match evaluateExpression scope expression with
        | ISuccess(scope', expression') ->
            // The assertion succeeded, but did it return true?
            if (debool(expression') = true) then
                // Yes, so return
                ISuccess(scope, expression')
            else
                // No, so fail as the assertion failed
                IFailure(createError scope.position "Assertion returned false.")
        | IFailure(f) ->
            IFailure(f)
// Evaluates an expression, takes a scope and expression to evaluate
and evaluateExpression scope expr = 
    match expr with
    // Expression is just a value, so return it in the interpreters 'boxed' form
    | Value (literal) ->
        match literal with
        | LBool (b) -> ISuccess(scope, Boolean_(b))
        | LInteger (i) -> ISuccess(scope, Int_(i))
        | LFloat (f) -> ISuccess(scope, Float_(f))
        | LString (s) -> ISuccess(scope, String_(s))
    // Expression is referring to a variable, so check it exists and return it if it does
    // .. if the variable doesn't exist, then fail
    | Identifier (id) ->
        if (scope.variables.ContainsKey(id)) then
            // Return the value of the variable being referened
            ISuccess(scope, scope.variables.[id].value)
        else
            IFailure(createError scope.position "Variable '" + id + "' does not exist.")
    // Beginning of a dot notation expression
    | InfixOp (lhs, Dot, rhs) ->
        // Evaluate left hand side of dot expression
        let left = evaluateExpression scope lhs
        match left with
        | ISuccess((scope, left)) ->
            // Call evaluateDotNotation with the left as the parent and right as the property
            evaluateDotNotation scope left rhs
        | IFailure(f) ->
            IFailure(f)
    // Infix expression
    | InfixOp (lhs, op, rhs) ->
        // Evaluate the left hand side
        let left = evaluateExpression scope lhs
        match left with
        | ISuccess s1 ->
            let (scope, _) = s1 // Extract the scope from the result
            // Evaluate the right with the extracted scope
            let right = evaluateExpression scope rhs
            match right with
            | ISuccess s2 ->
                // Call the infix op evaluater
                evaluateInfixOp scope s1 s2 op
            | IFailure f2 ->
                IFailure(f2)
        | IFailure f1 ->
            IFailure(f1)
    // Prefix expression
    | PrefixOp (op, rhs) ->
        // Evaluate the operand
        let right = evaluateExpression scope rhs
        match right with
        | ISuccess s ->
            // Apply the prefix operator
            evaluatePrefixOp scope s op
        | IFailure f ->
            IFailure(f)
    // Evaluate a typed variable declaration
    | TypedVarDecl (name, tname) ->
        // Check the type annotation refers to a valid type
        if (typeExists scope tname) then
            // Check to see if the type is a user type or an in-built type
            if (userTypeExists scope tname) then
                // Initialise an empty instance of the object
                let value = initialiseObject scope tname
                match value with
                | ISuccess(scope', value) ->
                    // Create a new variable with the name, type and value
                    let newVar = { name = name; varType = tname; value = value; }
                    if (scope.variables.ContainsKey(newVar.name)) then
                        // If the variable already exists, then return an error
                        IFailure(createError scope.position (sprintf "%s is already defined." newVar.name))
                    else
                        // The variable isn't defined, so add it to the scope
                        let newScope = { scope with variables = scope.variables.Add(name, newVar) }
                        ISuccess(newScope, value)
                | IFailure(f) ->
                    IFailure(f)
            else
                // Generate the default value for this type
                let value = defaultValue tname
                // Create a new variable with the default value
                let newVar = { name = name; varType = tname; value = value; }
                // Add to the scope, if it already exists return an error
                if (scope.variables.ContainsKey(newVar.name)) then
                    IFailure(createError scope.position (sprintf "%s is already defined." newVar.name))
                else
                    let newScope = { scope with variables = scope.variables.Add(name, newVar) }
                    ISuccess(newScope, value)
        else
            IFailure(createError scope.position (sprintf "%s is not a known type." tname))
    // Evaluating a typed variable declaration that has an expression
    | TypedVarDecl' ((name, tname), expr) ->
        // Make sure the type exists
        if (typeExists scope tname) then
            // Evaluate the value of the expression
            let value = evaluateExpression scope expr
            match value with
            | ISuccess (sc, v) ->
                // Make sure the value type is the same as the type annotation
                if ((valName v) = tname) then
                    // Create a new variable to hold this
                    let newVar = { name = name; varType = tname; value = v }
                    // If the variable already exists, return an error, otherwise
                    // return the new scope with the variable
                    if (scope.variables.ContainsKey(newVar.name)) then
                        IFailure(createError scope.position (sprintf "%s is already defined." newVar.name))
                    else
                        let newScope = { sc with variables = scope.variables.Add(name, newVar) }
                        ISuccess(newScope, v)
                else
                    IFailure (typeError scope.position tname (valName v))
            | IFailure f ->
                IFailure (f)
        else
            IFailure(createError scope.position (sprintf "%s is not a known type" tname))
    // Evaluating a variable declaration with no type annotation
    | UntypedVarDecl (name, expr) ->
        // Evaluate the value expression
        let value = evaluateExpression scope expr
        match value with
        | ISuccess(scope, result) ->
            // Create new variable with the value and name, get the type of the value to be the var type
            let newVar = { name = name; varType = valName result; value = result; }
            // Make sure a variable with the same name doesn't exist
            if (scope.variables.ContainsKey(newVar.name)) then
                IFailure(createError scope.position (sprintf "%s is already defined." newVar.name))
            else
                // Return new scope with variable added
                let newScope = { scope with variables = scope.variables.Add(name, newVar) }
                ISuccess(newScope, result)
        | IFailure f ->
            IFailure(f)
    // Evaluating a function call
    | FunctionCall (func, args) ->
        // Evaluate the argument expressions and store them as a list
        let evaluatedArgs = List.map (fun a -> match a with | Arg e -> evaluateExpression scope e) args
        // Call handleMethodCall
        handleMethodCall scope func evaluatedArgs
    // Handling an inline if expression
    | InlineIf (expr, caseTrue, caseFalse) ->
        // Evaluate the condition
        let condition = evaluateExpression scope expr
        match condition with
        | ISuccess (scope, result) ->
            // If the condition returns true, evaluate the left side of the if statement and
            // return its value. If the condition returns false, evaluate the right side of the
            // if statement and return its value.
            if (result = Boolean_(true)) then
                evaluateExpression scope caseTrue
            else
                evaluateExpression scope caseFalse
        | IFailure f ->
            IFailure (f)
    // Evaluating an array literal expression
    | ArrayLiteral (array) ->
        // Evaluate each member of the literal syntax
        let valList = List.map (fun x -> evaluateExpression scope x) array
        // Check to see if every value is evaluated successfully
        let succ = List.forall (fun x -> match x with | ISuccess (_,_) -> true | IFailure _ -> false) valList
        if (succ) then
            // Every value evaluated successfully, get the type of the first item to be used as the
            // type for the array
            let assumedType = match valList.[0] with
                              | ISuccess (_, value) -> valName value
            // Make sure every item is the same type as the array
            let typesMatch = List.forall (fun x -> match x with
                                                   | ISuccess (_, v) -> valName v = assumedType) valList
            if (typesMatch) then
                // Types match so get the values of all the items
                let r = List.map (fun x -> match x with | ISuccess(s, v) -> v) valList
                // Convert list to an array
                let a = r |> Array.ofList
                // Return the new array as the value
                ISuccess (scope, Array_(assumedType, a))
            else
                IFailure (createError scope.position (sprintf "Expected array item of type %s." assumedType))
        else
            // Get the list of errors
            let m = List.map (fun x -> match x with | ISuccess (_,_) -> "" | IFailure f -> f + "\n") valList
            let errs = List.reduce (fun x y -> x + y) m
            IFailure (errs)
    // Evaluating an array get expression
    | ArrayGet (a, idx) ->
        // Get the array
        let array = evaluateExpression scope a
        // Get the index
        let index = evaluateExpression scope idx
        match index with
        | ISuccess (s, i) ->
            match i with
            // Make sure index is an integer
            | Int_ (idx) ->
                match array with
                | ISuccess (s, v) ->
                    match v with
                    // Make sure 'v' is an array
                    | Array_(type', ls) ->
                        // Make sure the index is within the bounds of the array
                        if (ls.Length <= idx) then
                            IFailure (createError s.position "Index is out of bounds.")
                        else if (idx < 0) then
                            IFailure (createError s.position "Index cannot be less than zero.")
                        else
                            // Return the item at the index 
                            ISuccess (scope, ls.[idx])
                    | _ ->
                        IFailure (createError s.position (sprintf "%s is not an array." (valName v)))
                | IFailure f ->
                    IFailure f
            | _ ->
                IFailure (createError s.position "Array index must be an integer.")
        | IFailure f ->
            IFailure f
    // Array set expression - only used inside expressions
    | ArraySet (a, idx, v) ->
        // Get the array
        let array = evaluateExpression scope a
        // Get the index
        let index = evaluateExpression scope idx
        // Evaluate the value that is to be inserted into the array
        let newVal = evaluateExpression scope v
        match index with
        | ISuccess (s, i) ->
            // Check the index is an integer
            match i with
            | Int_ (idx) ->
                // Check the array is an array
                match array with
                | ISuccess (s, v) ->
                    match v with
                    | Array_(type', ls) ->
                        match newVal with
                        | ISuccess (s, nv) ->
                            // Make sure the type being assigned is the same type as the array
                            if (valName nv = type') then
                                // Check the index is within bounds of the array
                                if (ls.Length <= idx) then
                                    IFailure (createError s.position "Index was outside bounds of array.")
                                else if (idx < 0) then
                                    IFailure (createError s.position "Index cannot be less than zero.")
                                else
                                    // Set the array item
                                    Array.set ls idx nv
                                    ISuccess (s, Array_(type', ls))
                            else
                                IFailure(createError s.position (sprintf "Cannot put type %s in %s Array." (valName nv) type'))
                        | IFailure f ->
                            IFailure f
                    | _ ->
                        IFailure (createError s.position (sprintf "%s is not an array." (valName v)))
                | IFailure f ->
                    IFailure f
            | _ -> 
                IFailure (createError s.position "Array index must be an integer.")
        | IFailure f ->
            IFailure f
// Recursively descends a list of statements, evaluating them and
// feeding the result scope of one into the next
and handleStatementList scope (statements: Statement List) =
    if (statements.IsEmpty) then
        ISuccess(scope, Null_)
    else
        let statementResult = evaluateStatement scope statements.Head
        if (statements.Tail.IsEmpty) then
            statementResult
        else
            match statementResult with
            | ISuccess (scope, result) ->
                handleStatementList scope statements.Tail
            | IFailure f ->
                IFailure f
// Evaluates a method call, given a name and a list of arguments
and handleMethodCall scope mname (args: Result<Scope * Value> list) =
    if (userTypeExists scope mname && args.Length = 0) then
        // Call is to the default object initialiser (creates an empty object)
        let object' = initialiseObject scope mname
        match object' with
        | ISuccess(s, o) ->
            // Return the new empty object
            ISuccess(s, o);
        | IFailure(f) ->
            IFailure(f)
    else if (methodExists scope mname) then
        // A method with the same name exists
        // Try to find an argument that failed, if it did return it's failure and exit
        let argFailed = List.tryFind (function | IFailure(_) -> true | ISuccess(_,_) -> false) args
        match argFailed with
        | Some(f) ->
            f
        | None ->
            // No argument failed to evaluate, so get the types of the arguments
            let argTypes = List.map (function | ISuccess(_,v) -> valName v) args
            // Check to see if an overload exists with the same types as the arguments
            let overload = handleOverloads scope mname argTypes
            match overload with
            | ISuccess(s,m) ->
                match m with
                // A user-defined method exists with the overload
                | Method'(method') ->
                    let scope' = { scope with position = method'.position; }
                    // Merge the argument values and the parameter names
                    let argValues = List.map (function | ISuccess(_,v) -> v) args
                    let paramNames = List.map (function | BasicParameter(nm,_) -> nm) method'.parameters
                    let nameArgList = List.zip paramNames argValues
                    // Method that adds an argument to a new scope
                    let addArgToScope scope (name, arg) =
                        let newVar = { name = name; varType = valName arg; value = arg; }
                        let newVarMap = scope.variables.Remove(name).Add(name, newVar)
                        { scope with variables = newVarMap }
                    // Create a new scope with all the arguments, param name pairs included
                    let argScope = List.fold addArgToScope scope' nameArgList
                    // Execute the method body with the new scope
                    let methodResult = handleStatementList argScope method'.statements
                    match methodResult with
                    | ISuccess(s, v) ->
                        // Method evaluated successfully, now remove any variables that were created by the method
                        // Changes are allowed to stay however
                        let scopeVars = s.variables
                        let oldVars = scope.variables |> Map.toList
                        let varUpdate (vName, var) =
                            let argExisted = List.fold (fun s (name, _) -> s || vName = name) false nameArgList
                            if (not argExisted) then
                                (vName, scopeVars.[vName])
                            else
                                (vName, var)

                        let newVars = Map.ofList (List.map varUpdate oldVars)
                        // Return with altered scope, and the return value
                        ISuccess({ scope with variables = newVars }, v)
                    | IFailure(f) ->
                        IFailure(f)
                | InbuiltMethod(method') ->
                    // An inbuilt method exists with the overload, so get the argument values
                    let argValues = List.map (function | ISuccess(_,v) -> v) args
                    // Call the in-built method mappin and return succesfully 
                    ISuccess(scope, method'.mapping argValues)
            | IFailure(f) ->
                IFailure(f)
    else
        // The method is not declared
        IFailure (createError scope.position (sprintf "%s is not a method." mname))
// Recursive implementation of the for loop, takes a scope, a condition, an iterator,
// and the body of the for loop
and handleForLoop scope condition iterator body =
    // Evaluate the condition
    let conditionResult = evaluateExpression scope condition
    match conditionResult with
    | ISuccess(scope', conditionValue) ->
        match conditionValue with
        | Boolean_(b) ->
            // Check to see if the condition is true or false
            if (b) then
                // If the condition is true, then execute the for loop body
                ignore (handleStatementList scope' body)    
                // Evaluate the iterator statement
                let iteratorResult = evaluateStatement scope' iterator
                match iteratorResult with
                | ISuccess(scope', value) ->
                    // Move on to the next loop with a recursive call
                    handleForLoop scope' condition iterator body
                | IFailure(f) ->
                    IFailure(f)
            else
                // The condition failed, so we can end the loops
                // Returns Null to indicate the end of the statement
                ISuccess(scope', Null_)
        | _ ->
            IFailure(createError scope'.position "For loop condition must evaluate to a Boolean value.")
    | IFailure(f) ->
        IFailure(f)
// Initialises an empty instance of an object
and initialiseObject scope tname =
    // Check to make sure the type we're instantiating exists
    if (not(typeExists scope tname)) then
        IFailure (createError scope.position (sprintf "Type %s does not exist." tname))
    else    
        // Get the type
        let template = scope.types.[tname]
        match template with
        | UserType u ->
            // User type, so create a map of the properties with default values
            let objMap = Map.ofList (List.map (fun p -> (p.pName, p.pValue)) u.properties)
            ISuccess (scope, Object_(tname, objMap))
        | InbuiltType u ->
            // Inbuilt types cannot be instantiated like this
            IFailure (createError scope.position (sprintf "Cannot initialise in-built type %s." tname))
// Evaluates dot notation recursively, so you can access the properties of an object 
// in the properties of an object in the properties of an object.......
and evaluateDotNotation scope parent property =
    // The parent can either be an in-built type, or an object
    match parent with
    | Object_ (ptype, pmap) ->
        match property with
        // If the property is just an identifier
        | Identifier (s) ->
            // Check it exists
            if (pmap.ContainsKey(s)) then
                // Are we going to change it's value?
                if (scope.isAssignment) then
                    // Make sure the new value is the same type as the old one
                    if (valName pmap.[s] = valName scope.assignValue) then
                        // Return the new Object with the altered property
                        ISuccess(scope, Object_(ptype, pmap.Remove(s).Add(s, scope.assignValue)))
                    else
                        IFailure(createError scope.position (sprintf "Property %s is a different type to %s." s (valName scope.assignValue)))
                else
                    // No assignment, just a get, return the property
                    ISuccess(scope, pmap.[s])
            else
                IFailure(createError scope.position (sprintf "Object %s does not have a property %s." ptype s))
        // If the property is an array get...
        | ArrayGet (property, idx) ->
            match property with
            // The array name IS an identifier, only have to match with that
            | Identifier (property) ->
                // Check the parent object has the property
                if (pmap.ContainsKey(property)) then
                    // Get the array
                    let array' = pmap.[property]
                    // Evaluate the index of the array we are accessing
                    let idx = evaluateExpression scope idx
                    match idx with
                    | ISuccess(s, idx) ->
                        match idx with
                        // The index must be an integer value
                        | Int_(i) ->
                            match array' with
                            // The array must be an array
                            | Array_(tname, array') ->
                                // Make sure the index is within the bounds of the array
                                if (array'.Length < i) then
                                    IFailure(createError s.position "Index was outside bounds of array.")
                                else if (i < 0) then
                                    IFailure(createError s.position "Index cannot be less than zero.")
                                else
                                    // Index is valid, are we going to change something?
                                    if (scope.isAssignment) then
                                        // Check the new value and the array type are the same
                                        if (valName scope.assignValue = tname) then
                                            // Set the value at index i in the array to the value on the right of the assignment
                                            Array.set array' i s.assignValue
                                            // Return new Object with the updated array
                                            ISuccess(s, Object_(ptype, pmap.Remove(property).Add(property, Array_(tname, array'))))
                                        else
                                            IFailure(createError scope.position (sprintf "Cannot put type %s in %s Array." (valName s.assignValue) tname))
                                    else
                                        // No assignment, just fetching the value
                                        ISuccess(scope, array'.[i])
                            | _ ->
                                IFailure(createError s.position (sprintf "%s is not an Array." property))
                        | _ ->
                            IFailure(createError s.position "Array index must be an integer.")
                    | IFailure(f) ->
                        IFailure(f)
                else
                    IFailure(createError scope.position (sprintf "Object %s does not have a property %s." ptype property))
        // The parent is an infix expression with another dot!
        | InfixOp (lhs, Dot, rhs) ->
            match lhs with
            // Check the left hand side is an identifier
            | Identifier (left) ->
                // Check the parent contains a property with the name of the left hand side
                if (pmap.ContainsKey(left)) then
                    // Get the property
                    let newParent = pmap.[left]
                    // Are we assigning to this property?
                    if (scope.isAssignment) then
                        // Call another dot notation evaluation, with the new parent (the left hand side), and the new property (right hand side)
                        // This call to evaluateDotNotation will also check isAssignment, and will at some point change a property
                        // to the value we are assigning
                        let result = evaluateDotNotation scope newParent rhs
                        match result with
                        | ISuccess(scope', newChild) ->
                            // Return a new object with the updated property
                            ISuccess(scope, Object_(ptype, pmap.Remove(left).Add(left, newChild)))
                        | IFailure(f) ->
                            IFailure(f)
                    else
                        // No assignment, just getting the value, so call dot notation again until we can return something
                        evaluateDotNotation scope newParent rhs
                else
                    IFailure(createError scope.position (sprintf "Type %s does not have a property %s." ptype left))
            | _ ->
                IFailure(createError scope.position "Properties must be identifiers.")
        | _ ->
            IFailure(createError scope.position (sprintf "Dot notation can only be used with properties."))
    // The parent is a value
    | value ->
        let name = valName value
        // Get the type associated with this value
        let inbuiltType = if (name.StartsWith "[") then scope.types.["Array"] else scope.types.[name]
        match inbuiltType with
        // Check that the type is an in-built one
        | InbuiltType(ibType) ->
            match property with
            | Identifier(s) ->
                // Check to see if the in-built type has a getter with the same name as the property
                let getter = List.tryFind (fun (name, func) -> name = s) ibType.getters
                match getter with
                | Some(getter) ->
                    // If there is a getter with that name, return its value
                    let (name, func) = getter
                    ISuccess(scope, func value)
                | None ->
                    IFailure(createError scope.position (sprintf "Type %s does not have a property %s." name s))
            | _ ->
                IFailure(createError scope.position "Properties must be identifiers.")
        | _ ->
            IFailure(createError scope.position (sprintf "Unknown type %s." name))
// Generates a list of properties from a list of expressions
and generatePropertyList scope (exprList: Expression List) : Result<Scope * Property List> =
    // Generates a list of properties from the variable declarations
    let properties = List.map (fun expr -> match expr with
                                            | TypedVarDecl(varName, varType) ->
                                                ISuccess(scope, {pName = varName; pType = varType; pValue = defaultValue varType})
                                            | TypedVarDecl'((varName, varType), varInit) ->
                                                let initResult = evaluateExpression scope varInit
                                                match initResult with
                                                | ISuccess(scope, initResult) ->
                                                    if (valName initResult = varType) then
                                                        ISuccess(scope, { pName = varName; pType = varType; pValue = initResult})
                                                    else
                                                        IFailure(createError scope.position
                                                            (sprintf "Type mismatch in property %s, default value is type %s." varName (valName initResult)))
                                                | IFailure(f) ->
                                                    IFailure(f)
                                            | UntypedVarDecl(varName, varInit) ->
                                                let initResult = evaluateExpression scope varInit
                                                match initResult with
                                                | ISuccess(scope, initResult) ->
                                                    ISuccess(scope, { pName = varName; pType = valName initResult; pValue = initResult})
                                                | IFailure(f) ->    
                                                    IFailure(f)
                                            | _ ->
                                                IFailure(createError scope.position "Types can only contain properties (variable declarations).")
                                    ) exprList
    // Find any errors that happened generating the properties
    let firstError = List.tryFind (function 
                                        | IFailure(f) -> true
                                        | ISuccess(_,_) -> false) properties
    match firstError with
    | Some(f) ->
        match f with
        | IFailure(f) ->
            IFailure(f)
    | None ->
        let properties = List.map (function
                                        | ISuccess(s,v) -> v) properties
        ISuccess(scope, properties)

let rec evaluateProgram scope (statements: Statement List) =
    if (statements.IsEmpty) then
        ISuccess(scope, Null_)
    else
        let currStatement = statements.Head
        let result = evaluateStatement scope currStatement
        match result with
        | ISuccess(scope, value) ->
            if (statements.Tail.Length > 0) then
                evaluateProgram scope statements.Tail
            else
                result
        | IFailure f ->
            IFailure f
