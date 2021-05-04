open Docopt
open Domain
open Service

let DOC = """
ES-CLI Todo Demo
=============================================================

Usage:
  todolist add <item> --path=<path>
  todolist get --path=<path>
  todolist (-h | --help)
  todolist --version


Options:
  -h --help     Show this screen.
  --version     Show version.
"""

let VERSION = "version 1.0.0"

let impossible () =
  failwith "Impossible Destination"

module Cli =
    
    type CliRequest =
      | ServiceRequest of ServiceRequest
      | VersionRequest of string
      | HelpRequest of string

    type CliResponse = CliResponseString of string
    
    let getRequiredArg (args:Docopt.Arguments.Dictionary) (arg:string): string =
      match args.Item arg with
      | Docopt.Arguments.Argument s -> s
      | _ -> impossible ()


    let (|Help|_|) (args:Docopt.Arguments.Dictionary) =
      if args.ContainsKey("--help") then
        Some (DOC)
      else
        None


    let (|Version|_|) (args:Docopt.Arguments.Dictionary) =
      if args.ContainsKey("--version") then
        Some (VERSION)
      else
        None

    let (|Add|_|) (args:Docopt.Arguments.Dictionary) =
        if args.ContainsKey("add") then
          let item = getRequiredArg args "<item>"
          let path = getRequiredArg args "--path"
          Some (path , AddTodoCmd { Name=item; })
        else
            None

    let (|Get|_|) (args:Docopt.Arguments.Dictionary) =
        if args.ContainsKey("get") then
          let path = getRequiredArg args "--path"
          Some (path , GetAllTodosQuery )
        else
            None
    
    let parseCliArgs (args:Docopt.Arguments.Dictionary): CliRequest =
        match args with
        | Help h -> HelpRequest h
        | Version v -> VersionRequest v
        | Add (path, cmd) -> ServiceRequest ( CommandRequest (path, cmd) )
        | Get (path, query) -> ServiceRequest ( QueryRequest (path, query) )
        | _ -> impossible ()


    // helper function for readability
    let handleAndAdaptServiceRequest (request:ServiceRequest): string =
      let serviceResult = handleServiceRequest request
      match serviceResult with
      | CommandSuccess -> "Command Success"
      | CommandFailure s -> s
      | QuerySuccess s -> s
      | QueryFailure s -> s
    
    let handleCliRequest (request:CliRequest): CliResponse =
      match request with
      | HelpRequest v -> CliResponseString v
      | VersionRequest v -> CliResponseString v
      | ServiceRequest req -> CliResponseString (handleAndAdaptServiceRequest req)
    
    let printCliResponse (response:CliResponse): unit =
      let (CliResponseString s) = response
      printfn "%s" s

[<EntryPoint>]
let main argv =
  try
    Docopt(DOC).Parse(argv)
    |> Cli.parseCliArgs
    |> Cli.handleCliRequest
    |> Cli.printCliResponse
    0
  with ArgvException ex ->
    printfn "%A" ex
    -1
