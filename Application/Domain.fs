module Domain

type TodoItem = TodoItem of string

type TodoListError = TodoListError of string
exception TodoListException of string


let (|LessThanFiveItems|_|) (state: TodoItem list) =
    let listLength = List.length state
    if listLength < 5 then Some() else None

let validate (state: TodoItem list) : Result<TodoItem list, TodoListError> =
    match state with
    | LessThanFiveItems _ -> Ok state
    | _ -> Error(TodoListError "Too many items to do! Can't add any more")

let addTodoItem
    (state: TodoItem list)
    (item: TodoItem)
    : Result<TodoItem list, TodoListError> =
    let newState = state @ [ item ]
    validate newState
