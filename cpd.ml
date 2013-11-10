open Util

type cpd_line = string array * float

type cpd = {vars:string array;
            data:cpd_line list;
           }

let string_of_cpd {vars; data} : string =
  let vars = Array.to_list vars in
  let s_list =
    [Printf.sprintf "%s | " (hd vars)^
     String.concat ", " @: tl vars;
     "data:"]@
    List.map (fun (deps, p) ->
      let deps = Array.to_list deps in
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
    (* if it's the same dep var, add. otherwise add a cpd *)
    match acc with
    | x::xs when (x.vars.(0)) = var_name -> 
        {x with data = (Array.of_list(var_val::dep_vals), p)::x.data}::xs
    | xs ->
        {vars = Array.of_list(var_name::dep_names); 
         data = [Array.of_list(var_val::dep_vals), p]}::xs
  )
  []
  lines

(*  let var_nums = List.flatten @:
    list_mapi (fun (i, var) -> if List.mem var vars then [i] else []) cpd.vars *)

let cpd_find_idxs cpd (var_names:string list) = 
  let h = Hashtbl.create 10 in
  Array.iteri (fun i var -> Hashtbl.add h var i) cpd.vars;
  List.fold_left (fun acc var ->
    Hashtbl.find h var::acc)
    []
    var_names

let take_idxs take_idxs len_take xs =
  let take = Array.make len_take "" in
  let _ = List.fold_left (fun cnt i ->
    take.(cnt) <- xs.(i); cnt + 1
  ) 0 take_idxs in
  take

(* change from indices taken to indices dropped *)
let invert_idxs idxs count =
  let h = Hashtbl.create 10 in
  List.iter (fun i -> Hashtbl.add h i ()) idxs;
  fst @: iterate (fun (acc,i) ->
      if Hashtbl.mem h i then acc,i+1
      else i::acc, i+1) 
    ([],0) 
    count

let marginalize cpd vars =
  let idxs = cpd_find_idxs cpd vars in
  (* handle names *)
  let var_len = Array.length cpd.vars in
  let remain_idxs = invert_idxs idxs var_len in
  let remain_len = List.length remain_idxs in
  let var_names = take_idxs remain_idxs remain_len cpd.vars in
  (* handle data *)
  let h = Hashtbl.create 10 in
  List.iter (fun (var_vals, p) ->
    let shrunk_vars = take_idxs remain_idxs remain_len var_vals in
    (* check for shrunk vars in hashtable *)
    try 
      (* if we have them, update the probability *)
      let p' = Hashtbl.find h shrunk_vars in
      Hashtbl.replace h shrunk_vars @: p +. p'
    with Not_found -> (* instantiate *)
      Hashtbl.add h shrunk_vars p
  ) cpd.data;
  (* get back whole cpd *)
  let data = Hashtbl.fold (fun vars p acc -> (vars, p)::acc) h [] in
  {vars=var_names; data}

(* filter a cpd by adding evidence, setting a var to a value *)
let add_evidence cpd var_value_list =
  let var_list, value_list = List.split var_value_list in
  let idxs = cpd_find_idxs cpd var_list in
  let data = List.filter (fun (var_vals, p) ->
      List.for_all2 (fun idx v -> var_vals.(idx) = v) idxs value_list)
    cpd.data
  in
  {cpd with data}

(* special intersect returning common indices and diff indices*)
let intersect vars1 vars2 =
  let h = Hashtbl.create 10 in
  Array.iteri (fun i var -> Hashtbl.add h var i) vars1;
  let acc_idx1, acc_idx2, acc_other2 =
    fst @: Array.fold_left 
    (fun ((acc_idx1, acc_idx2, acc_other2),idx2) var2 ->
      try
        let idx1 = Hashtbl.find h var2 in
        Hashtbl.remove h var2;
        (idx1::acc_idx1, idx2::acc_idx2, acc_other2), idx2+1
      with Not_found -> 
        (acc_idx1, acc_idx2, idx2::acc_other2), idx2+1
    ) (([],[],[]),0) vars2
  in
  (* get the diff indices of vars1 using the fact that we removed the
   * common indices *)
  let acc_other1 = Array.fold_left (fun acc_other1 var1 ->
    try
      let idx1 = Hashtbl.find h var1 in
      idx1::acc_other1
    with Not_found -> acc_other1
  ) [] vars1
  in 
  acc_idx1, acc_idx2, acc_other1, acc_other2

(* condition a variable to be a certain value and retain the rest of the cpd *)
(* returns a hashtable on the conditioned variables *)
let condition cpd_data idxs_keys idxs_keys_len : (string array, cpd_line list) Hashtbl.t =
  let h = Hashtbl.create 10 in
  let len_vars = 
    let vars, p = hd cpd_data in
    Array.length vars
  in
  let idxs_vals = invert_idxs idxs_keys len_vars in
  let idxs_vals_len = len_vars - idxs_keys_len in
  List.iter (fun (vars, p) ->
    let keys = take_idxs idxs_keys idxs_keys_len vars in
    let vals = take_idxs idxs_vals idxs_vals_len vars in
    try
      let oldvals = Hashtbl.find h keys in
      Hashtbl.replace h keys @: (vals, p)::oldvals
    with Not_found -> Hashtbl.add h keys [vals, p]
  ) cpd_data;
  h

(* concatenate arrays of strings into one array *)
let concat_vars l_vars =
  let len = List.fold_left (fun acc arr -> acc + Array.length arr)
    0 l_vars in
  let new_arr = Array.create len "" in
  let _ = List.fold_left (fun acc_len arr ->
    let arr_len = Array.length arr in
    Array.blit arr 0 new_arr acc_len arr_len;
    acc_len + arr_len
  ) 0 l_vars in
  new_arr
    
(* get the join-based product of 2 cpds *)
let product cpd1 cpd2 =
  let idxs1, idxs2, diff_idxs1, diff_idxs2  = intersect cpd1.vars cpd2.vars in
  let l_common_idxs = List.length idxs1 in
  let l_diff_idxs1 = List.length diff_idxs1 in
  let l_diff_idxs2 = List.length diff_idxs2 in
  let vars_common = take_idxs idxs1 l_common_idxs cpd1.vars in
  let vars_diff1 = take_idxs diff_idxs1 l_diff_idxs1 cpd1.vars in
  let vars_diff2 = take_idxs diff_idxs2 l_diff_idxs2 cpd2.vars in
  let new_vars = concat_vars [vars_common; vars_diff1; vars_diff2] in
  let cpd1_c_hash = condition cpd1.data idxs1 l_common_idxs in
  let cpd2_c_hash = condition cpd2.data idxs2 l_common_idxs in
  let new_data =
    Hashtbl.fold (fun keys cpd1_d acc_cpd ->
      let cpd2_d = 
        try Hashtbl.find cpd2_c_hash keys with Not_found -> [] in
      let cross_product =
        List.fold_left (fun acc (vals1, p1) ->
          List.fold_left (fun acc' (vals2, p2) ->
            (concat_vars [keys;vals1;vals2], p1 *. p2)::acc'
          ) acc cpd2_d
        ) [] cpd1_d
      in
      cross_product@acc_cpd
    ) 
    cpd1_c_hash []
  in
  {vars=new_vars; data=new_data}

(* to do division, we loop over the common vars and divide the bigger factor
 * by the smaller one *)
let div cpd1 cpd2 =
  let idxs1, idxs2, diff_idxs1, diff_idxs2 = intersect cpd1.vars cpd2.vars in
  if not @: null diff_idxs2 then failwith "Div: second factor too big" else
  let l_common_idxs = List.length idxs1 in
  (* reason to use condition: it rearranges the hash by idx if needed *)
  let cpd2_c_hash = condition cpd2.data idxs2 l_common_idxs in
  (* we mostly keep the first cpd *)
  let new_data =
    List.rev_map (fun (vars, p1) ->
        let keys  = take_idxs idxs1 l_common_idxs vars in
        let _, p2 = try hd @: Hashtbl.find cpd2_c_hash keys
                    with Not_found -> failwith @: "div: missing value" in
        let div = match p1, p2 with
          | 0., 0. -> 0.
          | _,  0. -> failwith "div: divide by 0"
          | _,  _  -> p1 /. p2
        in
        vars, div)
     cpd1.data
  in
  {cpd1 with data=new_data}

(* ******* tests *************************************)

let test_cpd = {vars=[|"a";"b"|]; 
                data=[
                  [|"0";"0"|], 0.5;
                  [|"1";"0"|], 0.5;
                  [|"0";"1"|], 0.2;
                  [|"1";"1"|], 0.8;
                ]
                }

let test_cpd' = {vars=[|"c";"b"|]; 
                data=[
                  [|"0";"0"|], 0.3;
                  [|"1";"0"|], 0.6;
                  [|"0";"1"|], 0.7;
                  [|"1";"1"|], 0.;
                ]
                }

let condition_test () = condition test_cpd.data [1] 1
  
let product_test () = product test_cpd test_cpd'

let test_cpd2 = {vars=[|"a";"b";"c"|]; 
                data=[
                  [|"0";"0";"0"|], 0.5;
                  [|"0";"0";"1"|], 0.25;
                  [|"0";"1";"0"|], 0.2;
                  [|"0";"1";"1"|], 0.8;
                  [|"1";"0";"0"|], 0.75;
                  [|"1";"0";"1"|], 0.6;
                  [|"1";"1";"0"|], 0.4;
                  [|"1";"1";"1"|], 0.1;
                ]
                }

let test_cpd2' = {vars=[|"c";"d";"b"|]; 
                data=[
                  [|"0";"0";"0"|], 0.9;
                  [|"0";"0";"1"|], 0.15;
                  [|"0";"1";"0"|], 0.32;
                  [|"0";"1";"1"|], 0.45;
                  [|"1";"0";"0"|], 0.22;
                  [|"1";"0";"1"|], 0.55;
                  [|"1";"1";"0"|], 0.11;
                  [|"1";"1";"1"|], 0.0;
                ]
                }
  
let condition_test2 () = 
  let h = condition test_cpd2.data [0;2] 2 in
  Hashtbl.find h [|"0";"0"|], 
  Hashtbl.find h [|"0";"1"|], 
  Hashtbl.find h [|"1";"0"|], 
  Hashtbl.find h [|"1";"1"|]
  
let product_test2 () = product test_cpd2 test_cpd2'

let div_test () = div test_cpd2 test_cpd'

let marginalize_test () = marginalize test_cpd2' ["d";"c"]
let marginalize_test2 () = marginalize test_cpd2' ["c"]


