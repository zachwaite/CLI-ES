module Commands

open Events
open Domain

type Command = AddTodoCmd of AddTodoMsg

let init () = List.empty<TodoItem>

let evolve (state: TodoItem list) (evt: Event) : Result<TodoItem list, TodoListError> =
    match evt with
    | AddTodoEvt msg -> TodoItem msg.Name |> addTodoItem state

// TODO: see if I can bind here
let evolve'
    (stateResult: Result<TodoItem list, TodoListError>)
    (evt: Event)
    : Result<TodoItem list, TodoListError> =
    match stateResult with
    | Ok state -> evolve state evt
    | Error e -> Error e

let hydrate
    (state: TodoItem list)
    (events: Event list)
    : Result<TodoItem list, TodoListError> =
    events |> List.fold evolve' (Ok state)

let assertHydrate
    (state: TodoItem list)
    (events: Event list)
    : Result<Event list, TodoListError> =
    let rs = hydrate state events

    match rs with
    | Ok _ -> Ok events
    | Error e -> Error e

let decide (state: TodoItem list) (cmd: Command) : Result<Event list, TodoListError> =
    match state, cmd with
    | _, AddTodoCmd args -> [ AddTodoEvt args ] |> assertHydrate state
