open! Core

(* [get_credits] should take the contents of an IMDB page for an actor and
   return a list of strings containing that actor's main credits. *)
let filter_credit node =
  match Soup.attribute "class" node with
  | None -> false
  | Some str ->
    (match String.compare "ipc-metadata-list-summary-item__t" str with
     | 0 -> true
     | _ -> false)
;;

let get_credits contents : string list =
  let open Soup in
  parse contents
  $$ "a"
  |> to_list
  |> List.filter ~f:filter_credit
  |> List.map ~f:(fun li ->
    texts li |> String.concat ~sep:"" |> String.strip)
;;

let print_credits_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "given an IMDB page for an actor, print out a list of their main \
       credits"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_credits contents) ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"imdb commands"
    [ "print-credits", print_credits_command ]
;;
