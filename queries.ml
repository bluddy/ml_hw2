open Util
open CliqueTree
open Cpd

type assign = (string * string)

type query = { 
  p_of  : assign list;
  given : assign list;
}

let string_of_assignments ss =
  let ss = List.map (fun (k,v) -> Printf.sprintf "%s = %s" k v) ss in
  String.concat ", " ss

let parse_queries ~scheme file =
  let get_key_val str = 
    let xs = r_split "=" str in
    hd xs, at xs 1
  in
  let f = read_file file in
  let lines = lines f in
  List.map (fun line ->
    match r_split " " line with
    | [q; dep] -> 
        let qs = r_split "," q in
        let p_of = match scheme with
          | SumProduct -> List.map get_key_val qs
          | MaxProduct -> List.map (fun s -> s,"") qs
        in
        let deps = r_split "," dep in
        let given = List.map get_key_val deps in
        {p_of; given}

    | [q] -> 
        let qs = r_split "," q in
        let p_of = List.map get_key_val qs in
        {p_of; given=[]}
        
    | _ -> failwith "invalid query format"
  ) lines

(* apply evidence to the tree *)
let apply_evidence tree given =
  let h = Hashtbl.create 10 in
  List.iter (fun (k,v) -> Hashtbl.add h k v) given;
  tree_fold (fun _ node ->
    let add_vars, add_vals = 
      Array.fold_left (fun (acc_vars, acc_vals) var_name ->
        try
          let value = Hashtbl.find h var_name in
          Hashtbl.remove h var_name;
          var_name::acc_vars, value::acc_vals 
        with Not_found -> acc_vars, acc_vals)
      ([],[])
      node.node_cpd.vars
    in
    (*Printf.printf "Adding evidence to node %d\n" node.id;*)
    node.node_cpd <- add_evidence node.node_cpd add_vars add_vals;
    ()
  ) () tree

(* for escaping tree_fold early *)
exception Answer of float

(* find the answer in the tree for a given query *)
let find_answer tree query =
  let q_vars, q_values = List.split query in
  let q_vars_arr = Array.of_list q_vars in
  try
    tree_fold (fun _ node ->
      let _, _, q_diff_idx, node_diff_idx = intersect q_vars_arr node.node_cpd.vars in
      if q_diff_idx <> [] then None
      else 
        let cpd = marginalize node.node_cpd node_diff_idx in
        let cpd = normalize_and_real cpd in
        let cpd = add_evidence cpd q_vars q_values in
        let answer = List.fold_left (fun p_tot (_,p,_) -> p_tot +. p) 0. cpd.data in
        raise @: Answer(answer)
    ) None tree
  with Answer a -> Some a

let process_queries ~incremental stream_fn tree q_list =
  let answers = List.rev @: fst @:
    List.fold_left (fun (acc_answers, m_last_q) q ->
      let retractive () =
        reset_edges_all tree;
        restore_node_cpds tree;
        apply_evidence tree q.given;
        stream_fn tree;
        let answer = find_answer tree q.p_of in
        (answer::acc_answers, Some q)
      in
      match m_last_q with
      | Some last_q ->
          let d1 = diff last_q.given q.given = [] in
          let d2 = diff q.given last_q.given = [] in
          if d1 && d2 then (* repeat from last *)
            let answer = find_answer tree q.p_of in
            (answer::acc_answers, Some q)
          else if d1 && incremental then
            let delta_given = diff q.given last_q.given in
            reset_edge_mailboxes tree;
            apply_evidence tree delta_given;
            stream_fn tree;
            let answer = find_answer tree q.p_of in
            (answer::acc_answers, Some q)
          else (* retractive *)
            retractive ()

      | _ -> retractive ()) (* retractive. we must reset the tree *)
    ([], None)
    q_list
  in answers

let process_queries_max stream_fn tree q_list : (float * assign list) list =
  List.map (fun q ->
    let q_vars_arr = Array.of_list @: fst_many q.p_of in
    reset_edges_all tree;
    restore_node_cpds tree;
    apply_evidence tree q.given;
    let root = match maxproduct_find_root tree q_vars_arr with
      | None   -> failwith "couldn't find appropriate root"
      | Some r -> r
    in
    stream_fn tree root; (* upstream only *)
    let _, node_idxs, _, diff_idxs = 
      intersect q_vars_arr root.node_cpd.backptrs in
    let cpd = root.node_cpd in
    let cpd = normalize_and_real cpd in
    let cpd = marginalize_max cpd diff_idxs in
    if List.length cpd.data <> 1 then 
      failwith "data hasn't reduced to 1 line";
    let _, p, back_vals = hd cpd.data in
    let l = List.length node_idxs in
    let wanted_vals = Array.to_list @: 
      take_idxs node_idxs l back_vals in
    let wanted_var_names = Array.to_list @:
      take_idxs node_idxs l root.node_cpd.backptrs in
    p, list_zip wanted_var_names wanted_vals
  ) q_list








