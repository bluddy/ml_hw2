open Util

type cpd_line = string list * float

type cpd = {vars:string list;
            data:cpd_line list;
           }

let string_of_cpd {vars; data} : string =
  let s_list =
    [Printf.sprintf "%s | " (hd vars)^
     String.concat ", " @: tl vars;
     "data:"]@
    List.map (fun (deps, p) ->
      let dep_s = String.concat ", " @: tl deps in
      Printf.sprintf "%s | %s -> %f" (hd deps) dep_s p
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
    | x::xs when hd(x.vars) = var_name -> 
        {x with data = (var_val::dep_vals, p)::x.data}::xs
    | xs ->
        {vars = var_name::dep_names; data = [var_val::dep_vals, p]}::xs
  )
  []
  lines

(*  let var_nums = List.flatten @:
    list_mapi (fun (i, var) -> if List.mem var vars then [i] else []) cpd.vars *)

let cpd_find_idx cpd var_name =
  match find_idx var_name cpd.vars with
    | Some i -> i 
    | None -> invalid_arg "var not in cpd"

let marginalize cpd var =
  let idx = cpd_find_idx cpd var in
  let var_names = list_remove_idxs [idx] cpd.vars in
  let hash = Hashtbl.create 10 in
  List.iter (fun (var_vals, p) ->
    let shrunk_vars = list_remove_idxs [idx] var_vals in
    (* check for shrunk vars in hashtable *)
    try 
      (* if we have them, update the probability *)
      let p' = Hashtbl.find hash shrunk_vars in
      Hashtbl.replace hash shrunk_vars (p +. p')
    with Not_found -> (* instantiate *)
      Hashtbl.add hash shrunk_vars p
  ) cpd.data;
  (* get back whole cpd *)
  let data = Hashtbl.fold (fun vars p acc -> (vars,p)::acc) hash [] in
  {vars=var_names; data}

let add_evidence cpd var value =
  let idx = cpd_find_idx cpd var in
  let data = List.filter (fun (var_vals, _) ->
      at var_vals idx = value) 
    cpd.data
  in
  {cpd with data}

(* special intersect returning both strings and indices *)
let intersect vars1 vars2 =
  let hash = Hashtbl.create 10 in
  List.iteri (fun i var -> Hashtbl.add hash var i) vars1;
  fst @: List.fold_left (fun (((acc_vars, acc_idx1, acc_idx2) as acc),idx2) var2 ->
    try
      let idx1 = Hashtbl.find hash var2 in
      (var2::acc_vars, idx1::acc_idx1, idx2::acc_idx2), idx2+1
    with Not_found -> (acc, idx2+1)
  ) (([],[],[]),0) vars2

(* regular difference function for lists *)
(* order preserving on l1 *)
let diff l1 l2 =
  let hash = Hashtbl.create (List.length l2) in
  List.iter (fun x -> Hashtbl.replace hash x ()) l2;
  List.rev 
   (List.fold_left (fun acc x ->
      try 
        Hashtbl.find hash x; acc 
      with Not_found -> x::acc
    ) [] l1)

(* condition a variable to be a certain value and retain the rest of the cpd *)
(* returns a hashtable on the conditioned variables *)
let condition cpd sorted_idxs : (string list, cpd_line list) Hashtbl.t =
  let hash = Hashtbl.create 10 in
  List.iter (fun (vars, p) ->
    let keys, values = list_partition_idxs sorted_idxs vars in
    try
      let oldvals = Hashtbl.find hash keys in
      Hashtbl.replace hash keys ((values, p)::oldvals)
    with Not_found -> Hashtbl.add hash keys [values, p]
  ) cpd.data;
  hash
    
let product cpd1 cpd2 =
  let vars_common, indices1, indices2 = intersect cpd1.vars cpd2.vars in
  (* we need sorted indices to remove efficiently *)
  let sorted_idxs1 = List.sort (-) indices1 in
  let sorted_idxs2 = List.sort (-) indices2 in
  let var_diff1 = list_remove_idxs sorted_idxs1 cpd1.vars in
  let var_diff2 = list_remove_idxs sorted_idxs2 cpd2.vars in
  let new_vars = vars_common@var_diff1@var_diff2 in
  let cpd1_c_hash = condition cpd1 sorted_idxs1 in
  let cpd2_c_hash = condition cpd2 sorted_idxs2 in
  let new_data =
    Hashtbl.fold (fun keys cpd1_d acc_cpd ->
      let cpd2_d = 
        try Hashtbl.find cpd2_c_hash keys with Not_found -> [] in
      let cross_product =
        List.fold_left (fun acc (val_list1, p1) ->
          List.fold_left (fun acc' (val_list2, p2) ->
            (keys@val_list1@val_list2, p1 *. p2)::acc'
          ) acc cpd2_d
        ) [] cpd1_d
      in
      cross_product@acc_cpd
    ) 
    cpd1_c_hash []
  in
  {vars=new_vars; data=new_data}






  


