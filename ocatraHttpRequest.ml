open Lwt
open String
open OcatraCommon
open OcatraHttpCommon
open OcatraHttpCommon.Util

type t = {
  methd: Method.t;
  path: string;
  param: string Param.t;
  version: string;
  header: string Header.t;
  content: Content.t;
}

let re_for_path = Str.regexp "^[a-zA-Z0-9-_/]+"

let parse_path line =
  if Str.string_match re_for_path line 0 then
    let eq_pos = Str.match_end () in
    let path = sub line 0 eq_pos in
    let line_len = length line in
    let tbl = Param.create 4 in
    let tbl =
    if line_len > eq_pos && line.[eq_pos] = '?' then
      let qstart = eq_pos + 1 in
      let qstr = sub line qstart (line_len - qstart) in
      Util.parse_kv_joined_by_eq Param.replace tbl qstr
    else tbl
    in
    (path, tbl)
  else raise (HttpError Status.BadRequest)

let re_for_request_line =
  Str.regexp "\\([A-Z]+\\) +\\([^ ]+\\) +\\(HTTP\\/[0-9]\\.[0-9]\\)"

let parse_request_line line =
  log @@ "[parse_request_line] line=" ^ line;
  if Str.string_match re_for_request_line line 0 then
    let methd_str = Str.matched_group 1 line in
    let path_and_q_str = Str.matched_group 2 line in
    let version = Str.matched_group 3 line in
    let methd = Method.method_of_string methd_str in
    let (path, param) = parse_path path_and_q_str in
    (methd, path, param, version)
  else raise (HttpError Status.BadRequest)

let re_for_header_line = Str.regexp "\\([A-Za-z-]+\\) *: *\\([^ ]+\\)"

let parse_header_line line =
  if Str.string_match re_for_header_line line 0 then
    (Str.matched_group 1 line, Str.matched_group 2 line) 
  else raise (HttpError Status.BadRequest)

let parse_header = function
  | line::ls ->
    let (methd, path, param, version) = parse_request_line line in
    let tbl = 
      List.fold_left (fun tbl line -> 
        log @@ "[parse_header_line] " ^ line;
        let (key, value) = parse_header_line line in
        Header.replace tbl key value;
        tbl
      ) (Header.create 8) ls
    in
    {methd;
     path;
     param;
     version;
     header=tbl;
     content=Content.None}
  | _ -> http_error Status.BadRequest

let parse_request inch =
  log "[parse]";
  let rec _parse ls =
    (try Lwt_io.read_line inch with End_of_file -> return "") >>=
      fun line ->
        let len = length line in
        let line = if len > 0 && line.[len - 1] = '\r' then sub line 0 (len - 1) else line in
        log @@ "[input_line] length=" ^ (string_of_int @@ String.length line) ^ ", line=" ^ line;
        if line = "" then return (List.rev ls) else _parse (line::ls)
  in
  _parse [] >>=
    fun ls ->
      log "[parse_header]";
      let req = parse_header ls in
      log "[content create]";
      begin
        try
          let content_type = Header.find req.header "Content-Type" in
          let content_length = Header.find req.header "Content-Length" in
          Content.create (int_of_string content_length) inch content_type
        with Not_found -> (return Content.None)
      end >>=
        fun content ->
          return
            {methd=req.methd;
             path=req.path;
             param=req.param;
             version=req.version;
             header=req.header;
             content}

let keepalive version header =
  try 
    let connection = Header.find header "Connection" in
    connection <> "close"
  with Not_found -> version = "HTTP/1.1"

