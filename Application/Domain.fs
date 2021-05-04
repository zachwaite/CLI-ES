module Domain 

exception DomainException of string
type DomainError = DomainError of string


type TodoArgs = { Name:string; }

type Event = 
    | AddTodoEvt of TodoArgs

type Command =
    | AddTodoCmd of TodoArgs

type State = State of string list

let init () =
    State (List.empty<string>)


let (|LessThanFiveItems|_|) (state:State) =
    let listLength = state |> fun (State s) -> List.length s
    if listLength < 5 then
        Some ()
    else
        None

let appendTo (state:State) (event:Event): State =
    match event with
    | AddTodoEvt args -> state |> fun (State s) -> State (s @ [args.Name;])

// Event handlers
let evolve (state:State) (evt:Event): State =
    match state, evt with
    | LessThanFiveItems, _ -> evt |> appendTo state
    | _ -> raise (DomainException "max 5 items")


let hydrate (state:State) (events:Event list): State =
    events |> List.fold evolve state

let assertHydrate (state:State) (events:Event list): Result<Event list, DomainError> =
    try
        hydrate state events |> ignore
        Ok events
    with
    | DomainException(e) -> Error (DomainError e)


// Command handlers:
// Given a scenario, provide a list of expected events to assertHydrate,
// and it will try to hydrate with the prospective events. If it passes,
// the prospective events are returned, else an error is trapped and returned.
let decide (state:State) (cmd: Command): Result<Event list, DomainError> =
    match state, cmd with
    | _, (AddTodoCmd args) ->  [AddTodoEvt args;] |> assertHydrate state


// Read side
// Synchronous queries
type Query =
    | GetAllTodosQuery

type Projection =
    | AllEvents of string list

let fromEvent (event:Event): string =
    match event with
    | AddTodoEvt args -> args.Name

let eventsToProject (events:Event list): string list =
    events |> List.map fromEvent

let project (events:Event list) (query:Query): Projection =
    match query with
    | GetAllTodosQuery -> AllEvents (eventsToProject events)
