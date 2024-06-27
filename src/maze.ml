open! Core
module Row = Int
module Col = Int

module Location = struct
  type key = char [@@deriving compare, sexp]

  module T = struct
    type t = Row.t * Col.t [@@deriving compare, sexp]
  end

  (* This funky syntax is necessary to implement sets of [Connection.t]s.
     This is needed to defined our [Network.t] type later. Using this
     [Comparable.Make] functor also gives us immutable maps, which might come
     in handy later. *)
  include Comparable.Make (T)
  include T
end

let rec create_maze_map_cols ~map ~row_string ~col ~(row : int) =
  if col < String.length row_string
  then (
    let ch = String.get row_string col in
    let new_map = Map.add_exn map ~key:(row, col) ~data:ch in
    create_maze_map_cols ~map:new_map ~row_string ~col:(col + 1) ~row)
  else map
;;

let rec create_maze_map_rows ~map ~unsolved_maze_list ~row =
  match List.hd unsolved_maze_list with
  | Some row_string ->
    let new_map = create_maze_map_cols ~map ~row_string ~col:0 ~row in
    let new_maze_list = List.tl_exn unsolved_maze_list in
    create_maze_map_rows
      ~map:new_map
      ~unsolved_maze_list:new_maze_list
      ~row:(row + 1)
    (* let new_maze_list = List.tl unsolved_maze_list in let location_list =
       List.mapi unsolved_maze_list ~f:(fun ch i -> let map = Map.add map
       (row, i) ch); *)
    (* let new_map = Map.add_multi map location_list create_maze_map ~map
       ~unsolved_maze_list:new_maze_list ~row:(row + 1) *)
  | None -> map
;;

let rec solve_maze_rec ~map:maze_map ~start ~end ~cur_path ~path_stack : Location list = 
  match Stack.pop path_stack with 
  | Some cur_loc ->
    (
        if () then

    )
    | None -> cur_path
;;

(* Gameplan: create a map that takes a row, col and gets a char associated
   with the location create a dfs algorithm that creates a list of moves
   needed to get to the exit with the list of moves, create a new map or use
   a hash map that replaces the characters with an X or something *)
let solve_maze unsolved_maze_list : string list =
  let empty_map = Map.empty (module Location) in
  (* let maze_map = create_maze_map ~map:empty_map ~unsolved_maze_list in *)
  (* let maze_map = List.folding_mapi unsolved_maze_list empty_map
     List.foldi *)
  let maze_map =
    create_maze_map_rows ~map:empty_map ~unsolved_maze_list ~row:0
  in
  let maze_start = 0, 0 in
  let maze_end = 0, 0 in
  Map.iteri maze_map ~f:(fun ~key ~data ->
    print_s [%message (key : int * int)];
    print_s [%message (data : char)]);
  solve_maze_rec ~map:maze_map ~start:maze_start ~end:maze_end ~cur_path:[maze_start] ~path_stack
;;

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () ->
        let unsolved_maze_list =
          In_channel.read_lines (File_path.to_string input_file)
        in
        let solved_maze_list = solve_maze unsolved_maze_list in
        List.iter solved_maze_list ~f:(fun maze_line ->
          print_endline maze_line)]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
