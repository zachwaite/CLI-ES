module Filestore


type StoreError = StoreError of string

type StoreResult = Result<int * Events.Event list, StoreError>

let toEvent (raw: string) : Events.Event = Events.AddTodoEvt ( Domain.TodoItem raw )

let fromEvent (evt: Events.Event) : string =
    match evt with
    | Events.AddTodoEvt ( Domain.TodoItem item ) -> item

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
