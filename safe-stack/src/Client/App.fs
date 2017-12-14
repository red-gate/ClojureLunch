module App

open Elmish
open Elmish.React

open Fable.Helpers.React.Props
module R = Fable.Helpers.React
open Fable.PowerPack.Fetch


type Model = int option

type Msg = Increment | Decrement | Init of Result<int, exn>

let init () = 
  let model = None
  let cmd = 
    Cmd.ofPromise 
      (fetchAs<int> "/api/init") 
      [] 
      (Ok >> Init) 
      (Error >> Init)
  model, cmd

let update msg (model : Model) =
  let model' =
    match model,  msg with
    | Some x, Increment -> Some (x + 2)
    | Some x, Decrement -> Some (x - 1)
    | None, Init (Ok x) -> Some x
    | _ -> None
  model', Cmd.none

let safeComponents =
  let intersperse sep ls =
    List.foldBack (fun x -> function
      | [] -> [x]
      | xs -> x::sep::xs) ls []

  let components =
    [ 
      "Suave.IO", "http://suave.io" 
      "Fable"   , "http://fable.io"
      "Elmish"  , "https://fable-elmish.github.io/"
    ]
    |> List.map (fun (desc,link) -> R.a [ Href link ] [ R.str desc ] )
    |> intersperse (R.str ", ")
    |> R.span [ ]

  R.p [ ]
    [ R.strong [] [ R.str "SAFE Template" ]
      R.str " powered by: "
      components ]

let show = function
| Some x -> string x
| None -> "Loading..."


let view model dispatch =
  R.div []
    [ R.h1 [] [ R.str "SAFE Template" ]
      R.p  [] [ R.str "The initial counter is fetched from server" ]
      R.p  [] [ R.str "Press buttons to manipulate counter:" ]
      R.button [ OnClick (fun _ -> dispatch Decrement) ] [ R.str "-" ]
      R.div [] [ R.str (show model) ]
      R.button [ OnClick (fun _ -> dispatch Increment) ] [ R.str "+" ]
      safeComponents ]
  
#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
