open OcatraCommon
open OcatraHttpCommon
open OcatraHttpRequest
open OcatraHttpResponse

module Proc = struct
  type t = OcatraHttpRequest.t -> OcatraHttpResponse.t
end

let get_routes = ref (OcatraRoutes.init ())
let post_routes = ref (OcatraRoutes.init ())
let put_routes = ref (OcatraRoutes.init ())
let delete_routes = ref (OcatraRoutes.init ())

let get path f =
  get_routes := OcatraRoutes.bind !get_routes path f

let post path f =
  post_routes := OcatraRoutes.bind !post_routes path f

let put path f =
  put_routes := OcatraRoutes.bind !put_routes path f

let delete path f =
  delete_routes := OcatraRoutes.bind !delete_routes path f

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
        let proc = OcatraRoutes.find routes req.path in proc req
      with 
      | HttpError st -> create_response ~status:st
          (Content.TextPlain (Status.string_of_status st)) ()
      | Not_found -> create_response ~status:Status.NotFound
          (Content.TextPlain "Not found") ()
    )

let say = OcatraHttpResponse.create_response

let (++>) req = Param.find req.param

