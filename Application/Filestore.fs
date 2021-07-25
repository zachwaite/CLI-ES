module Filestore


type StoreError = StoreError of string

type StoreResult = Result<int * Events.Event list, StoreError>

let ensure_two_parts (parts:string[]): Result<string[],string> =
    if Array.length parts = 2 then
        Ok parts
    else
        Error "Wrong number of parts"

let (|AddTodo|_|) (parts: string[])=
    if parts.[0] = "AddTodoEvt" then
        Some ()
    else
        None

let extractEvent (parts: string[]): Result<Events.Event, StoreError> =
    match parts with
    | AddTodo _ -> Ok (Events.AddTodoEvt(({Key=parts.[0]; Value=parts.[1];})))
    | _ -> Error (StoreError "Unable to classify stored event")
    

let toEvent (raw: string) : Result<Events.Event, StoreError> =
    let partsResult = ensure_two_parts (raw.Split(","))
    match partsResult with
    | Ok parts ->  extractEvent parts
    | Error e -> Error (StoreError e)
    

let fromEvent (evt: Events.Event) : string =
    match evt with
    | Events.AddTodoEvt  item  -> $"{item.Key},{item.Value}"

let getAllEvents (path: string) : Events.Event list =
    System.IO.File.ReadLines(path)
    |> Seq.map toEvent
    |> Seq.toList

let saveEvents (path: string) (events: Events.Event list) : StoreResult =
    let eventCount = List.length events

    let eventStrings =
        events |> List.map fromEvent |> List.toArray

    System.IO.File.WriteAllLines(path, eventStrings)
    Ok(eventCount, events)
