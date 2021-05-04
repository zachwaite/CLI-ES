module Service

open Domain
open Filestore

// open Views

type ServiceRequest =
    | CommandRequest of path:string * Command
    | QueryRequest of path:string * Query

type ServiceResponse =
    | CommandSuccess
    | CommandFailure of string
    | QuerySuccess of string
    | QueryFailure of string

let handleCommand (path:string) (command:Command):  ServiceResponse =
    let history = getAllEvents path
    let state = history |> hydrate (init ())
    let decisionResult = decide state command
    match decisionResult with
    | Ok events ->
        let allEvents = history @ events
        let storageResult = saveEvents path  allEvents
        match storageResult with
        | Ok _ -> CommandSuccess
        | Error err ->
            match err with
            | (StoreError e) -> CommandFailure e
    | Error err ->
        match err with
        | DomainError e -> CommandFailure e


let handleQuery (path:string) (query:Query): ServiceResponse =
    let history = getAllEvents path
    let projection = project history query
    match projection with
    | AllEvents eventList -> eventList |> Seq.ofList |> String.concat "," |> QuerySuccess


let handleServiceRequest (request:ServiceRequest): ServiceResponse =
    // CommandFailure "This command failed"
    match request with
    | CommandRequest (path, cmd) -> handleCommand path cmd
    | QueryRequest (path, cmd) -> handleQuery path cmd

// let updateViews (repo:RepoConnection) (position:int) (events:Event list): Result<_, _> =
//     failwith "TODO"
// 
// let main (store:StoreConnection) (repo:RepoConnection) (command:Command): Result<_,_> =
//     handleCommand |> Result.bind updateViews

