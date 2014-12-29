type 'a t = (string * Str.regexp * 'a) list

let init () = []

let remove_tail_slath path =
  if path = "/" then path
  else begin
    let len = String.length path in
    if path.[len - 1] = '/' then
      String.sub path 0 (len - 1)
  else
    path
  end

let bind routes path proc =
  let path = remove_tail_slath path in
  let re = Str.regexp ("^" ^ path ^ "\\b") in
  let new_route = (path, re, proc)::routes in
  List.sort (fun (path0, _, _) (path1, _, _) -> compare path1 path0) new_route

let find routes path = 
  if path = "/" then
    let (_, _, proc) =
      List.find (fun (p, _, _) -> p = path) routes in
    proc
  else
    let path = remove_tail_slath path in
    let (_, _, proc) =
      List.find (fun (_, re, _) -> 
        Str.string_match re path 0) routes in
    proc
