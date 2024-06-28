open! Core
module Row = Int
module Col = Int

module Location = struct
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
  | None -> map
;;

let in_bounds ~cur_row ~cur_col ~num_rows ~num_cols =
  cur_row < num_rows && cur_row >= 0 && cur_col < num_cols && cur_col >= 0
;;

let find_neighbors ~map ~loc ~num_rows ~num_cols ~visited : Location.t list =
  match loc with
  | row, col ->
    let potential_list =
      [ row + 1, col; row - 1, col; row, col + 1; row, col - 1 ]
    in
    (* print_endline "Potential list: "; List.iter potential_list ~f:(fun
       location -> print_s [%message (location : int * int)]); *)
    let new_list =
      List.filter potential_list ~f:(fun cur_loc ->
        match cur_loc with
        | cur_row, cur_col ->
          in_bounds ~cur_row ~cur_col ~num_rows ~num_cols
          && (not (Char.equal (Map.find_exn map cur_loc) '#'))
          && not
               (Set.exists visited ~f:(fun path_loc ->
                  Location.equal cur_loc path_loc)))
    in
    (* print_endline "new list after find neighbors"; List.iter new_list
       ~f:(fun location -> print_s [%message (location : int * int)]); *)
    new_list
;;

let rec solve_maze_rec
  ~map:maze_map
  ~maze_end
  ~num_rows
  ~num_cols 
  ~cur_loc
  ~visited
  : Location.t list option
  =
  (* print_endline "Stack: inside rec "; Stack.iter path_stack ~f:(fun
     location -> print_s [%message (location : int * int)]); *)
  if Location.equal cur_loc maze_end
  then Some [ maze_end ]
  else (
    let valid_neighbors =
      find_neighbors ~map:maze_map ~loc:cur_loc ~num_rows ~num_cols ~visited
    in
    if List.is_empty valid_neighbors
    then None
    else
      List.find_map valid_neighbors ~f:(fun neighbor_loc ->
        match
          solve_maze_rec
            ~map:maze_map
            ~maze_end
            ~num_rows
            ~num_cols
            ~cur_loc:neighbor_loc
            ~visited:(Set.add visited cur_loc)
        with
        | Some loc_list -> Some ([ cur_loc ] @ loc_list)
        | None -> None))
;;

let find_char ~maze_map ~ch : Location.t =
  let key_list = Map.keys maze_map in
  List.find_exn key_list ~f:(fun key ->
    Char.equal (Map.find_exn maze_map key) ch)
;;

(* Gameplan: create a map that takes a row, col and gets a char associated
   with the location create a dfs algorithm that creates a list of moves
   needed to get to the exit with the list of moves, create a new map or use
   a hash map that replaces the characters with an X or something *)
let solve_maze unsolved_maze_list : string list =
  let num_rows = List.length unsolved_maze_list in
  let num_cols = String.length (List.hd_exn unsolved_maze_list) in
  let empty_map = Map.empty (module Location) in
  (* let maze_map = create_maze_map ~map:empty_map ~unsolved_maze_list in *)
  (* let maze_map = List.folding_mapi unsolved_maze_list empty_map
     List.foldi *)
  let maze_map =
    create_maze_map_rows ~map:empty_map ~unsolved_maze_list ~row:0
  in
  let maze_start = find_char ~maze_map ~ch:'S' in
  let maze_end = find_char ~maze_map ~ch:'E' in
  let visited = Set.empty (module Location) in
  (* *)
  (* print_endline "starting neighbors: "; List.iter starting_neighbors
     ~f:(fun location -> print_s [%message (location : int * int)]); *)
  (* *)
  (* testing *)
  (* print_endline "Stack: outside rec "; Stack.iter path_stack ~f:(fun
     location -> print_s [%message (location : int * int)]); *)
  (* Map.iteri maze_map ~f:(fun ~key ~data -> print_s [%message (key : int *
     int)]; print_s [%message (data : char)]); *)
  (* end testing*)
  let loc_list =
    match
      solve_maze_rec
        ~map:maze_map
        ~cur_loc:maze_start
        ~maze_end
        ~visited
        ~num_rows
        ~num_cols
    with
    | Some thing -> thing
    | None -> []
  in
  (* print_endline "Solution: "; List.iter loc_list ~f:(fun location ->
     print_s [%message (location : int * int)]); *)
  List.mapi unsolved_maze_list ~f:(fun map_row maze_str ->
    String.mapi maze_str ~f:(fun map_col ch ->
      if List.exists loc_list ~f:(fun loc ->
           Location.equal loc (map_row, map_col))
         && (not (Char.equal ch 'S'))
         && not (Char.equal ch 'E')
      then ' '
      else ch))
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
