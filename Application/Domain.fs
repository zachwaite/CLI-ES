module Domain

type TodoItem = TodoItem of string
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
