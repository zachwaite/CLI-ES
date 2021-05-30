module Commands

open Events
open Domain

type Command = AddTodoCmd of AddTodoMsg
type DecisionResult = Result<Event list, TodoListError>

let init () = List.empty<TodoItem>

let evolve (state: TodoItem list) (evt: Event) : TodoResult =
    match evt with
    | AddTodoEvt msg -> TodoItem msg.Name |> addTodoItem state

let evolve' (stateResult: TodoResult) (evt: Event) : TodoResult =
    match stateResult with
    | Ok state -> evolve state evt
    | Error e -> Error e

let hydrate (state: TodoItem list) (events: Event list) : TodoResult =
    events |> List.fold evolve' (Ok state)

let assertHydrate (state: TodoItem list) (events: Event list) : DecisionResult =
    let rs = hydrate state events

    match rs with
    | Ok _ -> Ok events
    | Error e -> Error e

let decide (state: TodoItem list) (cmd: Command) : DecisionResult =
    match state, cmd with
    | _, AddTodoCmd args -> [ AddTodoEvt args ] |> assertHydrate state
