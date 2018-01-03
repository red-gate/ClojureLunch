open System.IO

open System.Net

open Suave
open Suave.Operators
open Suave.Json
open System.Runtime.Serialization
let path = Path.Combine("..","Client") |> Path.GetFullPath 
let port = 8085us

let config =
  { defaultConfig with 
      homeFolder = Some path
      bindings = [ HttpBinding.create HTTP (IPAddress.Parse "0.0.0.0") port ] }

let init =
  42
  |> string
  |> Successful.OK

let items =
  ["hello"; "clive"]
  |> List.toArray
  |> toJson
  |> Utils.ASCII.toString
  |> Successful.OK


let hello =
  "Hello world"
  |> Successful.OK

let getItems =
  System.String.Join(", ", ["fruit1", "fruit2"])
  |> Successful.OK

let mutable randomizer = System.Random()

let mutable count = 1

let sleep milliseconds message: WebPart =
  fun (x : HttpContext) ->
    do count <- count + 1
    Successful.OK (count |> string) x
    
    //async {
      //do! Async.Sleep milliseconds
      //do count <- count + 1
      //return! Successful.OK (count |> string) x
    //}
    

let webPart =
  choose [
    Filters.path "/api/elements" >=> getItems
    Filters.path "/api/hello" >=> hello
    Filters.path "/api/init" >=> init
    Filters.path "/api/items" >=> items
    Filters.path "/api/random" >=> sleep 3000  "haahha"
    Files.browseHome
  ]

startWebServer config webPart