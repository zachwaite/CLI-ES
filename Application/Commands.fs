module Commands

open Events
open Domain

type Command =
    | AddTodoCmd of TodoItem
type DecisionResult = Result<Event list, DomainError>

let evolve (state: TodoItem list) (evt: Event) : DomainResult =
    match evt with
    | AddTodoEvt item -> item |> addTodoItem state


let initialState () = List.empty<TodoItem>

let hydrate (init: TodoItem list) (events: Event list) : DomainResult =
    let liftEvolve rs evt =
        match rs with
        | Ok state -> evolve state evt
        | Error err -> Error err

    events |> List.fold liftEvolve (Ok init)

let assertHydrate (state: TodoItem list) (events: Event list) : DecisionResult =
    let rs = hydrate state events

    match rs with
    | Ok _ -> Ok events
    | Error e -> Error e

let decide (state: TodoItem list) (cmd: Command) : DecisionResult =
    match state, cmd with
    | _, AddTodoCmd args -> [ AddTodoEvt args ] |> assertHydrate state
