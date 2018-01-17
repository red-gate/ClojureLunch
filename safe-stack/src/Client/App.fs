module App

open Elmish
open Elmish.React

open Fable.Helpers.React.Props
module R = Fable.Helpers.React
open Fable.PowerPack.Fetch



type Model = (int option * string list)

type Msg = Increment 
          | Decrement 
          | Init of Result<int, exn> 
          | InitItems of Result<string list, exn>
          | ItemSelected of string
let init () = 
  
  let model = None, ["Blag"]

  let cmdInt = 
    Cmd.ofPromise 
      (fetchAs<int> "/api/init") 
      [] 
      (Ok >> Init) 
      (Error >> Init)

  let cmdBag = 
    Cmd.ofPromise 
      (fetchAs<string list> "/api/items") 
      [] 
      (Ok >> InitItems) 
      (Error >> InitItems) 

  model, Cmd.batch(seq {
    yield cmdInt
    yield cmdBag })



let removeItemFromBag item bag = List.filter (fun x -> x <> item) bag

let addToBag counter item bag = 
  let cmdAdd = 
      Cmd.ofPromise 
        (postRecord "/api/addToBag" item) 
        [] 
        (Ok >> fun _ _ -> ()) 
        (Error >> fun _ _ -> ())

  let cmd = seq{ yield cmdAdd } |> Cmd.batch 

  (counter, removeItemFromBag item bag), cmd

let update msg (model : Model) =
  let (model', cmd') =
    match model,  msg with
    | (Some x, bar), Increment -> (Some (x + 2), bar), Cmd.none
    | (Some x, bar), Decrement -> (Some (x - 1) , bar), Cmd.none
    | (None, bar), Init (Ok x) -> ((Some x, bar)), Cmd.none
    | (i, _), InitItems (Ok x) -> ((i, x)), Cmd.none
    | (i, bag), ItemSelected (item) -> addToBag i item bag
    | _ -> (None, []), Cmd.none
  model', cmd'


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

let viewBagItem dispatch item  = 
  R.li [] [
    R.str item
    R.button [OnClick (fun _ -> dispatch (ItemSelected item))] [R.str "Add to bag"]
  ]

let view (intmodel, bag) dispatch =
  R.div []
    [ R.h1 [] [ R.str "Super market" ]
      R.p  [] [ R.str "The initial counter is fetched from server" ]
      R.p  [] [ R.str "Press buttons to manipulate counter:" ]
      R.button [ OnClick (fun _ -> dispatch Decrement) ] [ R.str "-" ]
      R.div [] [ R.str (show intmodel) ]
      R.button [ OnClick (fun _ -> dispatch Increment) ] [ R.str "+" ]
      R.br []
      R.ul [] (List.map (viewBagItem dispatch) bag)
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
