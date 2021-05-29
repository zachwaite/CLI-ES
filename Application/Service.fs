module Service

open Domain
open Commands
open Queries

type ServiceRequest =
    | CommandRequest of path: string * Command
    | QueryRequest of path: string * Query

type ServiceResponse =
    | CommandSuccess
    | CommandFailure of string
    | QuerySuccess of string
    | QueryFailure of string

let responseToString (response: ServiceResponse) : string =
    match response with
    | CommandSuccess -> "Success"
    | CommandFailure s
    | QuerySuccess s
    | QueryFailure s -> s

let handleCommand (path: string) (command: Command) : ServiceResponse =
    let history = Filestore.getAllEvents path
    let hydrateStateResult = history |> hydrate (init ())

    match hydrateStateResult with
    | Ok state ->
        let decisionResult = decide state command

        match decisionResult with
        | Ok events ->
            let storageResult =
                Filestore.saveEvents path (history @ events)

            match storageResult with
            | Ok _ -> CommandSuccess
            | Error (Filestore.StoreError err) -> CommandFailure err
        | Error (TodoListError err) -> CommandFailure err
    | Error (TodoListError err) -> CommandFailure err

let handleQuery (path: string) (query: Query) : ServiceResponse =
    let history = Filestore.getAllEvents path
    let projection = project history query

    match projection with
    | AllEvents eventList ->
        eventList
        |> Seq.ofList
        |> String.concat "\n"
        |> QuerySuccess

let handleServiceRequest (request: ServiceRequest) : ServiceResponse =
    match request with
    | CommandRequest (path, cmd) -> handleCommand path cmd
    | QueryRequest (path, cmd) -> handleQuery path cmd
