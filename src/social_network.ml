open! Core
module Person = String

(* We separate out the [Network] module to represent our social network in
   OCaml types. *)
module Network = struct
  (* We can represent our social network graph as a set of connections, where
     a connection represents a friendship between two people. *)
  module Connection = struct
    module T = struct
      type t = Person.t * Person.t [@@deriving compare, sexp]
    end

    (* This funky syntax is necessary to implement sets of [Connection.t]s.
       This is needed to defined our [Network.t] type later. Using this
       [Comparable.Make] functor also gives us immutable maps, which might
       come in handy later. *)
    include Comparable.Make (T)
    include T

    let of_string s =
      match String.split s ~on:',' with
      | [ x; y ] -> Some (Person.of_string x, Person.of_string y)
      | _ -> None
    ;;
  end

  type t = Connection.Set.t [@@deriving sexp_of]

  let of_file input_file =
    let connections =
      In_channel.read_lines (File_path.to_string input_file)
      |> List.concat_map ~f:(fun s ->
        match Connection.of_string s with
        | Some (a, b) ->
          (* Friendships are mutual; a connection between a and b means we
             should also consider the connection between b and a. *)
          [ a, b; b, a ]
        | None ->
          printf
            "ERROR: Could not parse line as connection; dropping. %s\n"
            s;
          [])
    in
    Connection.Set.of_list connections
  ;;
end

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing friendships and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file listing all friendships"
      in
      fun () ->
        let network = Network.of_file input_file in
        (* This special syntax can be used to easily sexp-serialize values
           (whose types have [sexp_of_t] implemented). *)
        printf !"%{sexp: Network.t}\n" network]
;;

(* In order to visualize the social network, we use the ocamlgraph library to
   create a [Graph] structure whose vertices are of type [Person.t].

   The ocamlgraph library exposes lots of different ways to construct
   different types of graphs. Take a look at
   https://github.com/backtracking/ocamlgraph/blob/master/src/imperative.mli
   for documentation on other types of graphs exposed by this API. *)
module G = Graph.Imperative.Graph.Concrete (Person)

(* We extend our [Graph] structure with the [Dot] API so that we can easily
   render constructed graphs. Documentation about this API can be found here:
   https://github.com/backtracking/ocamlgraph/blob/master/src/dot.mli *)
module Dot = Graph.Graphviz.Dot (struct
    include G

    (* These functions can be changed to tweak the appearance of the
       generated graph. Check out the ocamlgraph graphviz API
       (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli)
       for examples of what values can be set here. *)
    let edge_attributes _ = [ `Dir `None ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing friendships and generate a graph visualizing \
       the social network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file listing all friendships"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        let network = Network.of_file input_file in
        let graph = G.create () in
        Set.iter network ~f:(fun (person1, person2) ->
          (* [G.add_edge] auomatically adds the endpoints as vertices in the
             graph if they don't already exist. *)
          G.add_edge graph person1 person2);
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* let find_friends_recurse network ~person ~(visited : Person.t list) :
   Person.t list = ignore (network : Network.t); ignore visited; ignore
   (person : Person.t); failwith "TODO" ;; *)
let compare_connection
  (connection : Network.Connection.t)
  ~(person : Person.t)
  =
  match connection with
  | a, b ->
    (match String.equal person a with true -> Some (a, b) | false -> None)
;;

let find_friends network ~person ~(visited : Person.t list) : Person.t list =
  let tuple_list =
    Set.to_list
      (Set.filter_map
         (module Network.Connection)
         network
         ~f:(compare_connection ~person))
  in
  List.filter_map tuple_list ~f:(fun person_tuple ->
    match person_tuple with
    | _a, b ->
      (match List.exists visited ~f:(fun name -> String.equal b name) with
       | false -> Some b
       | true -> None))
;;

let rec find_friend_group_rec
  network
  ~person
  ~visited
  ~(person_q : Person.t Queue.t)
  : Person.t list
  =
  (* print_s [%message (visited : string list)]; print_s [%message (person_q
     : Person.t Queue.t)]; *)
  match Queue.dequeue person_q with
  | Some new_person ->
    let new_visited = List.append visited [ new_person ] in
    Queue.enqueue_all person_q (find_friends network ~person ~visited);
    new_visited
    @ find_friend_group_rec
        network
        ~person:new_person
        ~visited:new_visited
        ~person_q
  | None -> []
;;

(* [find_friend_group network ~person] returns a list of all people who are
   mutually connected to the provided [person] in the provided [network]. *)
let find_friend_group network ~person : Person.t list =
  let (person_q : Person.t Queue.t) = Queue.create () in
  (* make this a set*)
  (* let visited = Set.empty (module Person) in let visited = Set.add visited
     person; *)
  let visited = [ person ] in
  Queue.enqueue_all person_q (find_friends network ~person ~visited);
  (* find_friend_group_rec network ~person ~visited ~person_q *)
  List.dedup_and_sort
    (find_friend_group_rec network ~person ~visited ~person_q)
    ~compare:(fun a b -> String.compare a b)
;;

(* start with a person, get their connections, enqueue those connections to a
   queue while the queue isn't empty, find all unique connections, enqueue
   them into the queue Idea: to get all friends for a single person, find
   everyone theyre connected with, then use List.exists or something with the
   list filter function to filter out seen people *)

(* find_friends_recurse network ~person ~visited:[] *)

let find_friend_group_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"given a person, find their entire friend group"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file listing all friendships"
      and person =
        flag
          "person"
          (required string)
          ~doc:"STRING name of person whose friend group to find"
      in
      fun () ->
        let network = Network.of_file input_file in
        let friends = find_friend_group network ~person in
        List.iter friends ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"social network commands"
    [ "load", load_command
    ; "visualize", visualize_command
    ; "find-friend-group", find_friend_group_command
    ]
;;
