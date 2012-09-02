open OcatraCommon
open OcatraHttpCommon
open OcatraHttpRequest
open OcatraHttpResponse

module Proc = struct
  type t = OcatraHttpRequest.t -> OcatraHttpResponse.t
end

module Routes = struct
  type t = (string, Proc.t) Hashtbl.t
  let init () : t = Hashtbl.create 10
  let bind = Hashtbl.replace
  let find = Hashtbl.find
end

let get_routes = Routes.init ()
let post_routes = Routes.init ()
let put_routes = Routes.init ()
let delete_routes = Routes.init ()

let get path f = Routes.bind get_routes path f
let post path f = Routes.bind post_routes path f
let put path f = Routes.bind put_routes path f
let delete path f = Routes.bind delete_routes path f

let run ?(port=59876) () =
  OcatraHttpServer.start port
    (fun req ->
      let route =
        match req.methd with
        | Method.Get -> get_routes
        | Method.Post -> post_routes
        | Method.Put -> put_routes
        | Method.Delete -> delete_routes
      in
      try
        let proc = Routes.find route req.path in proc req
      with 
      | HttpError st -> create_response ~status:st
          (Content.TextPlain (Status.string_of_status st)) ()
      | Not_found -> create_response ~status:Status.NotFound
          (Content.TextPlain "Not found") ()
    ) ()

let say = OcatraHttpResponse.create_response

let (++>) req = Param.find req.param

