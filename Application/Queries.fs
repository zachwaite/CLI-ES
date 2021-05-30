module Queries

open Events

type Query = | GetAllTodosQuery

type Projection =
    AllEvents of string list

let fromEvent (index: int) (event: Event) : string =
    match event with
    | AddTodoEvt args -> $"{index + 1}: {args.Name}"

let eventsToProject (events: Event list) : string list =
    events |> List.mapi fromEvent

let project (events: Event list) (query: Query) : Projection =
    match query with
    | GetAllTodosQuery -> AllEvents(eventsToProject events)
