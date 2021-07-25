module Domain

type TodoItem = { Key: string; Value: string }
type DomainError = DomainError of string
type DomainResult = Result<TodoItem list, DomainError>


let (|LessThanFiveItems|_|) (state: TodoItem list) =
    let listLength = List.length state
    if listLength < 5 then Some() else None

let validate (state: TodoItem list) : DomainResult =
    match state with
    | LessThanFiveItems _ -> Ok state
    | _ -> Error(DomainError "Too many items to do! Can't add any more")

let addTodoItem (state: TodoItem list) (item: TodoItem) : DomainResult =
    let newState = state @ [ item ]
    validate newState

let removeTodoItemByKey (state: TodoItem list) (key: string): DomainResult =
    let maybeFound = state |> List.tryFind (fun item -> item.Key = key)
    match maybeFound with
    | Some _ -> validate (state |> List.filter (fun item -> item.Key <> key))
    | None -> Error ( DomainError "Key not found" )
    
    