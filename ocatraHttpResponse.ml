open String
open OcatraHttpCommon
open OcatraHttpCommon.HttpUtil

type t = {
  status: HttpStatus.t;
  header: string HttpHeader.t;
  content: HttpContent.t;
}

let create_response ?(status=HttpStatus.OK) ?(header=HttpHeader.create 0) content () =
  {status; header; content}

let response out_ch res =
  let status = res.status in
  let header = res.header in
  let content = res.content in
  log "[response]";
  output_string out_ch
    ("HTTP/1.1 " ^ HttpStatus.string_of_status status ^ "\r\n");
  HttpHeader.iter
    (fun k v -> output_string out_ch @@ k ^ ": " ^ k ^ "\r\n") header;
  match content with
  | HttpContent.None -> ()
  | c -> begin
    output_string out_ch @@ "Content-Type: " ^ 
      HttpContent.string_of_content_type c ^ "\r\n";
    let body = HttpContent.string_of_content_body c in
    output_string out_ch @@ "Content-Length: " ^ 
      string_of_int (length body) ^ "\r\n";
    output_string out_ch "\r\n";
    output_string out_ch body
  end;
  flush out_ch

