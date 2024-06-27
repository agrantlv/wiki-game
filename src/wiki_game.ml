open! Core
module Article = String
  (* We can represent our social network graph as a set of connections, where
     a connection represents a friendship between two people. *)
  module Connection = struct
    module T = struct
      type t = Article.t * Article.t [@@deriving compare, sexp]
    end

    (* This funky syntax is necessary to implement sets of [Connection.t]s.
       This is needed to defined our [Network.t] type later. Using this
       [Comparable.Make] functor also gives us immutable maps, which might
       come in handy later. *)
    include Comparable.Make (T)
    include T
    let hash _t = 1 

  end


(* [get_linked_articles] should return a list of wikipedia article lengths contained in
   the input.

   Note that [get_linked_articles] should ONLY return things that look like wikipedia
   articles. In particular, we should discard links that are:
   - Wikipedia pages under special namespaces that are not articles (see
     https://en.wikipedia.org/wiki/Wikipedia:Namespaces)
   - other Wikipedia internal URLs that are not articles
   - resources that are external to Wikipedia
   - page headers

   One nice think about Wikipedia is that stringent content moderation results in
   uniformity in article format. We can expect that all Wikipedia article links parsed
   from a Wikipedia page will have the form "/wiki/<TITLE>". *)
let get_linked_articles contents : string list =
  let open Soup in
  let string_option_list = 
  parse contents
  $$ "a"
  |> to_list
  |> List.map ~f:(fun li ->
    attribute "href" li) in
  List.filter_map string_option_list ~f:(fun li->
    match li with 
    | None -> None
    | Some str -> match (Wikipedia_namespace.namespace str) with 
        | None -> 
          (match ( String.is_prefix ~prefix:"/wiki/" str) with
          | true -> Some str
          | false -> None)
        | _ -> None
    ) |> List.dedup_and_sort ~compare:String.compare   
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;


(* NOT SURE IF CORRECT *)
module G = Graph.Imperative.Graph.Concrete (String)

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



let clean_token website : string =
  let no_first_slash = match (String.chop_prefix website ~prefix:"/") with
  | Some str -> str
  | None -> website
in
let no_wiki = match (String.chop_prefix no_first_slash ~prefix:"wiki/") with
      | Some new_str -> new_str
      | None -> no_first_slash
in
String.filter no_wiki ~f:(fun ch -> ((Char.is_alphanum ch) || (Char.is_whitespace ch)))
;;



(* GAMEPLAN:
   get list of linked articles from contents BOOM
   iterate over list, calling visualize_rec and adding +1 to the current_depth BOOM
   during recursion, add link to set (figure out how to get name later) BOOM
 *)
let rec visualize_rec ~max_depth ~current_depth ~visited ~origin ~how_to_fetch () : unit = 
  if current_depth > max_depth then ()
  else
    let contents = File_fetcher.fetch_exn how_to_fetch ~resource:origin in
    let linked_articles_list = get_linked_articles contents in
    List.iter linked_articles_list ~f:(fun article ->
      Hash_set.add visited (origin, article);
      if not (Hash_set.exists visited ~f:(fun article_from_set -> 
        match article_from_set with 
        | (a, _b) -> 
        String.equal a article)) then
        visualize_rec ~max_depth ~current_depth:(current_depth + 1) ~visited ~origin:article ~how_to_fetch ()
      );
;;

(* [visualize] should explore all linked articles up to a distance of [max_depth] away
   from the given [origin] article, and output the result as a DOT file. It should use the
   [how_to_fetch] argument along with [File_fetcher] to fetch the articles so that the
   implementation can be tested locally on the small dataset in the ../resources/wiki
   directory. *)
   (* origin: original name of wikipedia article 
      output file: file where to put result in
      how to fetch: kinda confused. 
   *)
let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  (* ignore (max_depth : int);
  ignore (origin : string);
  ignore (output_file : File_path.t);
  ignore (how_to_fetch : File_fetcher.How_to_fetch.t);
  failwith "TODO" *)
  (* let contents = File_fetcher.fetch_exn how_to_fetch ~resource:origin in
  let visited = Hash_set.create (module Connection) in
  Hash_set.add visited origin;
  visualize_rec ~max_depth ~current_depth:0 ~visited ~contents ~how_to_fetch (); *)
  let visited = Hash_set.create (module Connection) in
  visualize_rec ~max_depth ~current_depth:0 ~visited ~origin ~how_to_fetch ();



  let graph = G.create () in
  Hash_set.iter visited ~f:(fun (website1, website2) ->
      let new_website_1 = 
        clean_token 
        website1 in
      let new_website_2 = 
        clean_token 
        website2 in
          (* [G.add_edge] auomatically adds the endpoints as vertices in the
             graph if they don't already exist. *)
      G.add_edge graph new_website_1 new_website_2);
  Dot.output_graph (Out_channel.create (File_path.to_string output_file)) graph;
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing the highway \
       network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* [find_path] should attempt to find a path between the origin article and the
   destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with [File_fetcher] to fetch
   the articles so that the implementation can be tested locally on the small dataset in
   the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the graph. *)
let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
  ignore (max_depth : int);
  ignore (origin : string);
  ignore (destination : string);
  ignore (how_to_fetch : File_fetcher.How_to_fetch.t);
  failwith "TODO"
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Play wiki game by finding a link between the origin and destination pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination = flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
