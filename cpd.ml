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

