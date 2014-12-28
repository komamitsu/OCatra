open OcatraCommon
open OcatraHttpCommon
open OcatraHttpRequest
open OcatraHttpResponse

module Proc = struct
  type t = OcatraHttpRequest.t -> OcatraHttpResponse.t
end

(* TODO: move this module to a new file *)
module Routes = struct
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
end

let get_routes = ref (Routes.init ())
let post_routes = ref (Routes.init ())
let put_routes = ref (Routes.init ())
let delete_routes = ref (Routes.init ())

let get path f =
  get_routes := Routes.bind !get_routes path f

let post path f =
  post_routes := Routes.bind !post_routes path f

let put path f =
  put_routes := Routes.bind !put_routes path f

let delete path f =
  delete_routes := Routes.bind !delete_routes path f

let run ?conf:(conf=OcatraConfig.create ()) () =
  OcatraHttpServer.start conf
    (fun req ->
      let routes =
        match req.methd with
        | Method.Get -> !get_routes
        | Method.Post -> !post_routes
        | Method.Put -> !put_routes
        | Method.Delete -> !delete_routes
      in
      try
        let proc = Routes.find routes req.path in proc req
      with 
      | HttpError st -> create_response ~status:st
          (Content.TextPlain (Status.string_of_status st)) ()
      | Not_found -> create_response ~status:Status.NotFound
          (Content.TextPlain "Not found") ()
    )

let say = OcatraHttpResponse.create_response

let (++>) req = Param.find req.param

