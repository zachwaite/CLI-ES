module Queries

open Events

type Query = | GetAllTodosQuery

type Projection = AllEvents of string list

let fromEvent (event: Event) : string =
    match event with
    | AddTodoEvt args -> args.Name

let eventsToProject (events: Event list) : string list = events |> List.map fromEvent

let project (events: Event list) (query: Query) : Projection =
    match query with
    | GetAllTodosQuery -> AllEvents(eventsToProject events)
