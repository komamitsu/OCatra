open Lwt
open String
open OcatraCommon
open OcatraHttpCommon
open OcatraHttpCommon.Util

type t = {
  status: Status.t;
  header: string Header.t;
  content: Content.t;
}

let create_response ?(status=Status.OK) ?(header=Header.create 0) content () =
  {status; header; content}

let response out_ch res =
  let status = res.status in
  let header = res.header in
  let content = res.content in
  let header_buf = Buffer.create 4096 in
  Header.iter
    (fun k v -> Buffer.add_string header_buf @@ k ^ ": " ^ k ^ "\r\n") header;
  log "[response]";
  Lwt_io.write out_ch ("HTTP/1.1 " ^ Status.string_of_status status ^ "\r\n") >>
  Lwt_io.write out_ch @@ Buffer.contents header_buf >>
  match content with
  | Content.None -> return_unit
  | c ->
    Lwt_io.write out_ch @@
      "Content-Type: " ^ Content.string_of_content_type c ^ "\r\n" >>
    let body = Content.string_of_content_body c in
    Lwt_io.write out_ch @@
      "Content-Length: " ^ string_of_int (length body) ^ "\r\n\r\n" >>
    Lwt_io.write out_ch body >>
    Lwt_io.flush out_ch

