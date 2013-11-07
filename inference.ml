open Util

type node = {id: int;
             scope: string list;
             mutable edges: node list}

let string_of_node {id; scope; edges} = 
  let scope_s = String.concat ", " scope in
  let edge_ids = List.map (fun x -> string_of_int x.id) edges in
  let edges_s = String.concat "," edge_ids in
  Printf.sprintf "id: %d\nscope: %s\nedges: %s\n" id scope_s edges_s

let tree_fold fn init tree =
  let rec loop acc node m_last_id =
    let newacc = fn acc node in
    (* loop over the edges *)
    List.fold_left (fun acc' node' ->
      match m_last_id with
      (* don't go back to where we came from *)
      | Some last_id when node'.id = last_id -> acc'
      | _ -> loop acc' node' @: Some node.id
    ) 
    newacc
    node.edges
  in loop init tree None

let string_of_tree tree =
  tree_fold (fun acc n -> acc^string_of_node n) "" tree

let parse_clique_tree file =
  let f = read_file file in
  let ls = lines f in
  let num_nodes = int_of_string @: hd ls in
  let ls = list_drop 1 ls in
  let node_lines = list_take num_nodes ls in
  let edge_lines = list_drop num_nodes ls in
  let node_tbl = Hashtbl.create 10 in
  let count = ref 1 in
  let node_l = ref [] in
  (* add to hashtable *)
  List.iter (fun line ->
      let scope = r_split "," line in
      let new_node = {id=(!count); scope; edges=[]} in
      Hashtbl.add node_tbl line new_node;
      node_l := new_node::(!node_l);
      count := !count + 1
    ) node_lines;
  List.iter (fun edge -> 
    match r_split " -- " edge with
    | [x; y] -> 
        begin try
          let x_node = Hashtbl.find node_tbl x in
          let y_node = Hashtbl.find node_tbl y in
          x_node.edges <- y_node::x_node.edges;
          y_node.edges <- x_node::y_node.edges;
        with Not_found -> failwith @: "Can't find "^x^" or "^y^" in hash table" end
    | _ -> failwith "Bad split"
  ) edge_lines;
  list_last !node_l
  
type cpd_line = string * string list * float

type cpd = {var:string;
            deps:string list;
            data:cpd_line list;
           }

let string_of_cpd {var; deps; data} : string =
  let s_list =
    [Printf.sprintf "%s | " var;
    String.concat ", " deps;
    "data:"]@
    List.map (fun (lhs, rhs_deps, p) ->
      let dep_s = String.concat ", " rhs_deps in
      Printf.sprintf "%s | %s -> %f" lhs dep_s p
    ) data
  in
  String.concat "\n" s_list

let string_of_cpd_list cs : string = 
  String.concat "\n\n" @: List.map string_of_cpd cs

let parse_cpd file =
  (* parse key=val *)
  let get_key_val str = 
    let xs = r_split "=" str in
    hd xs, at xs 1
  in
  let f = read_file file in
  let lines = lines f in
  (* accumulate cpds *)
  List.fold_left (fun acc line ->
    let elems = r_split " " line in
    let var_name, var_val = get_key_val @: hd elems in
    (* get dependencies *)
    let dep_list = r_split "," @: at elems 1 in
    let dep_names, dep_vals = List.split @: 
      List.map (fun str -> get_key_val str) dep_list in
    (* get prob value *)
    let p = float_of_string @: at elems 2
    in
    (* if it's the same var, add. otherwise add a cpd *)
    match acc with
    | x::xs when x.var = var_name -> 
        {x with data = (var_val, dep_vals, p)::x.data}::xs
    | xs ->
        {var = var_name; deps=dep_names; data = [var_val, dep_vals, p]}::xs
  )
  []
  lines

type actions = Print_Tree
             | Print_CPDs
             | Inference

type params_t = {
  mutable action: actions;
  mutable network_file: string;
  mutable cpd_file: string;
  mutable cliquetree_file: string;
  mutable queries_file: string;
}

let params = {
  action=Inference;
  network_file="";
  cpd_file="";
  cliquetree_file="";
  queries_file="";
}

let parse_cmd_line () =
  let files = ref [] in
  let param_specs = Arg.align
   ["--print_cpds", Arg.Unit (fun () -> params.action <- Print_CPDs),
     "Print the CPDs";
    "--print_tree", Arg.Unit (fun () -> params.action <- Print_Tree),
     "Print the clique tree";
   ]
  in
  Arg.parse param_specs
    (fun f -> files := !files @ [f]) (* add files *)
    "network_file cpd_file cliquetree_file queries_file";
  match !files with 
  | [nf;cpd;ct;q] -> params.network_file    <- nf;
                     params.cpd_file        <- cpd;
                     params.cliquetree_file <- ct;
                     params.queries_file    <- q
  | _ -> print_endline 
    "Please specify network_file cpd_file cliquetree_file queries_file";
    exit 1

let run () = 
  let tree = parse_clique_tree params.cliquetree_file in
  let cpd_list = parse_cpd params.cpd_file in
  match params.action with
  | Print_CPDs -> print_endline @: string_of_cpd_list cpd_list
  | Print_Tree -> print_endline @: string_of_tree tree
  | _          -> failwith "NA"

let _ = 
  parse_cmd_line ();
  run ();


