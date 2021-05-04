module Filestore

open Domain

type StoreError = StoreError of string

type StoreResult = Result<int * Event list, StoreError>

let toEvent (raw:string): Event =
    AddTodoEvt { Name=raw; }

let fromEvent (evt:Event): string =
    match evt with
    | AddTodoEvt args -> args.Name
    
let getAllEvents (path:string): Event list =
    System.IO.File.ReadLines(path)
    |> Seq.map toEvent
    |> Seq.toList
    
let saveEvents (path:string) (events:Event list): StoreResult =
    let eventCount = List.length events
    let eventStrings = events |> List.map fromEvent |> List.toArray
    System.IO.File.WriteAllLines(path, eventStrings) |> ignore
    Ok (eventCount, events)

