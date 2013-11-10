open Util
open Cpd

type edge = {
  node: node;
  mutable sepset: string array;
  mutable cpd: cpd
}

and node = {id: int;
             scope: string list;
             mutable edges: edge list}

let string_of_node {id; scope; edges} = 
  let scope_s = String.concat ", " scope in
  let edge_ids = List.map (fun {node;_} -> string_of_int node.id) edges in
  let edges_s = String.concat "," edge_ids in
  Printf.sprintf "id: %d\nscope: %s\nedges: %s\n" id scope_s edges_s

let tree_fold fn init tree =
  let rec loop acc node m_last_id =
    let newacc = fn acc node in
    (* loop over the edges *)
    List.fold_left (fun acc' {node=node';_} ->
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
          x_node.edges <- {node=y_node; sepset=[||]; cpd=empty_cpd}::x_node.edges;
          y_node.edges <- {node=x_node; sepset=[||]; cpd=empty_cpd}::y_node.edges;
        with Not_found -> failwith @: "Can't find "^x^" or "^y^" in hash table" end
    | _ -> failwith "Bad split"
  ) edge_lines;
  list_last !node_l

