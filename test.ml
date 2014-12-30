open Lwt
open OUnit
open String
open OcatraCommon
open OcatraHttpCommon

module HttpRequestTest = struct
  open OcatraHttpRequest

  (* don't include "Content-Length" in header *)
  let test_request header body f =
    let filename = "test_request.txt" in
    let och = open_out filename in
    output_string och header;
    begin match body with
      | None -> ()
      | Some content -> begin
          let len = String.length content in
          output_string och @@ "Content-Length: " ^ string_of_int len ^ "\r\n";
          output_string och "\r\n";
          output och content 0 len
        end
    end;
    close_out och;

    ignore (
      Lwt_io.open_file Lwt_io.input filename >>=
        fun inch ->
          f inch;
          Lwt_io.close inch
    );
    Unix.unlink filename

  let test_parse_ok0 () =
    let header =
      "GET /index.html HTTP/1.1\r\n" ^
      "Host:example.com\r\n"
    in
    test_request header None
    (fun inch ->
      parse_request inch >>=
        fun req ->
          assert_equal Method.Get req.methd;
          assert_equal "/index.html" req.path;
          assert_equal "HTTP/1.1" req.version;
          assert_equal "example.com" (Header.find req.header "Host");
          assert_equal Content.None req.content;
          return_unit
    )

  let test_parse_ok1 () =
    let header =
      "GET /hoge/foo/bar?name=zxcv&age=123 HTTP/1.1\r\n" ^
      "Host:example.com\r\n" ^
      "Cookie : sessionid=1qaz2wsx3edc4rf\r\n"
    in
    test_request header None
    (fun inch ->
      parse_request inch >>=
        fun req ->
          assert_equal Method.Get req.methd;
          assert_equal "/hoge/foo/bar" req.path;
          assert_equal "HTTP/1.1" req.version;
          assert_equal "zxcv" (Param.find req.param "name");
          assert_equal "123" (Param.find req.param "age");
          assert_equal "example.com" (Header.find req.header "Host");
          assert_equal "sessionid=1qaz2wsx3edc4rf" (Header.find req.header "Cookie");
          assert_equal Content.None req.content;
          return_unit
    )

  let test_parse_ok2 () =
    let header =
      "POST /hoge/foo/bar HTTP/1.1\r\n" ^
      "Host:example.com:8080\r\n" ^
      "Content-Type: application/x-www-form-urlencoded\r\n" ^
      "Cookie : sessionid=1qaz2wsx3edc4rf\r\n" in
    let body = Some "name=komamitsu&age=73&blood=x" in
    test_request header body
    (fun inch ->
      parse_request inch >>=
        fun req ->
          assert_equal Method.Post req.methd;
          assert_equal "/hoge/foo/bar" req.path;
          assert_equal "HTTP/1.1" req.version;
          assert_equal "example.com:8080" (Header.find req.header "Host");
          assert_equal "sessionid=1qaz2wsx3edc4rf" (Header.find req.header "Cookie");
          match req.content with
          | Content.ApplicationXWwwFormUrlencoded params ->
              assert_equal 3 (Param.length params);
              assert_equal "komamitsu" (Param.find params "name");
              assert_equal "73" (Param.find params "age");
              assert_equal "x" (Param.find params "blood");
              return_unit
          | _ -> failwith "wrong content_type"
    )

  let test_parse_ng_comma_missing () =
    let header =
      "GET /hoge/foo/bar?name=zxcv&age=123 HTTP/1.1\r\n" ^
      "Host:example.com\r\n" ^
      "Cookie sessionid=1qaz2wsx3edc4rf"
    in
    test_request header None
    (fun inch ->
      let test () = parse_request inch in
      assert_raises (HttpError Status.BadRequest) test
    )

  let test_keepalive () =
    let header = Header.create 4 in
    assert_equal true @@ keepalive "HTTP/1.1" header;
    assert_equal false @@ keepalive "HTTP/1.0" header;
    Header.replace header "Connection" "close";
    assert_equal false @@ keepalive "HTTP/1.1" header;
    assert_equal false @@ keepalive "HTTP/1.0" header;
    Header.replace header "Connection" "Keep-Alive";
    assert_equal true @@ keepalive "HTTP/1.1" header;
    assert_equal true @@ keepalive "HTTP/1.0" header
 
  let suite = 
    "http_request" >:::
      [
        "test_parse_ok0" >:: test_parse_ok0;
        "test_parse_ok1" >:: test_parse_ok1;
        "test_parse_ok2" >:: test_parse_ok2;
        "test_parse_ng_comma_missing" >:: test_parse_ng_comma_missing;
        "test_keepalive" >:: test_keepalive;
      ]
end

module HttpResponseTest = struct
  open OcatraHttpResponse

  let test_response ?(status=Status.OK) ?(header=Header.create 0) content f =
    let filename = "test_response.txt" in
    ignore (
      Lwt_io.open_file Lwt_io.output filename >>=
        (fun och -> 
          response och @@ create_response ~status:status ~header:header content () >>
          Lwt_io.close och) >>= 
        (fun _ ->
          let inch = open_in filename in
          (f inch);
          close_in inch;
          Unix.unlink filename;
          Lwt.return_unit)
    )

  let test_ok1 () =
    test_response Content.None (fun inch ->
      assert_equal "HTTP/1.1 200 OK\r" @@ input_line inch
    )

  let test_ok2 () =
    let body = "Hello, World" in
    test_response (Content.TextPlain body)
      (fun inch ->
        assert_equal "HTTP/1.1 200 OK\r" @@ input_line inch;
        assert_equal "Content-Type: text/plain\r" @@ input_line inch;
        assert_equal ("Content-Length: " ^ string_of_int (length body) ^ "\r") @@ input_line inch;
        assert_equal "\r" @@ input_line inch;
        assert_equal body @@ input_line inch
      )

  let test_ng_not_found () =
    test_response ~status:Status.NotFound Content.None (fun inch ->
      assert_equal "HTTP/1.1 404 Not Found\r" @@ input_line inch
    )

  let suite = 
    "http_response" >:::
      [
        "test_ok1" >:: test_ok1;
        "test_ok2" >:: test_ok2;
        "test_ng_not_found" >:: test_ng_not_found;
      ]
end

module OcatraRoutesTest = struct
  let test_bind_and_find () =
    let r = OcatraRoutes.init () in
    let r = OcatraRoutes.bind r "/foo" 0 in
    let r = OcatraRoutes.bind r "/foo/abc/xyz" 1 in
    let r = OcatraRoutes.bind r "/foo/abc" 2 in
    let r = OcatraRoutes.bind r "/" 3 in
    let r = OcatraRoutes.bind r "/bar/hello" 4 in
    let r = OcatraRoutes.bind r "/bar/" 5 in
    let r = OcatraRoutes.bind r "/reg/.*/exp" 6 in
    let r = OcatraRoutes.bind r "/re/.*" 7 in
    assert_equal (OcatraRoutes.find r "/foo/abc/xyz") @@ 1;
    assert_equal (OcatraRoutes.find r "/foo/abc/xyz") @@ 1;
    assert_equal (OcatraRoutes.find r "/foo/abc") @@ 2;
    assert_equal (OcatraRoutes.find r "/foo/abc/") @@ 2;
    assert_equal (OcatraRoutes.find r "/foo/") @@ 0;
    assert_equal (OcatraRoutes.find r "/foo") @@ 0;
    assert_equal (OcatraRoutes.find r "/") @@ 3;
    assert_equal (OcatraRoutes.find r "/bar/hello/") @@ 4;
    assert_equal (OcatraRoutes.find r "/bar/hello") @@ 4;
    assert_equal (OcatraRoutes.find r "/bar/") @@ 5;
    assert_equal (OcatraRoutes.find r "/bar") @@ 5;
    assert_equal (OcatraRoutes.find r "/reg/foo/bar/exp") @@ 6;
    assert_equal (OcatraRoutes.find r "/re/hello/world") @@ 7;
    assert_raises (Not_found) (fun _ -> OcatraRoutes.find r "/foo/abc/xyz/000");
    assert_raises (Not_found) (fun _ -> OcatraRoutes.find r "/foo/abc/xxx");
    assert_raises (Not_found) (fun _ -> OcatraRoutes.find r "/foo/abcd");
    assert_raises (Not_found) (fun _ -> OcatraRoutes.find r "/foox");
    assert_raises (Not_found) (fun _ -> OcatraRoutes.find r "/fo");
    assert_raises (Not_found) (fun _ -> OcatraRoutes.find r "/bar/hello/you");
    assert_raises (Not_found) (fun _ -> OcatraRoutes.find r "/bar/hello/you/");
    assert_raises (Not_found) (fun _ -> OcatraRoutes.find r "/bar/hell");
    assert_raises (Not_found) (fun _ -> OcatraRoutes.find r "/barx");
    assert_raises (Not_found) (fun _ -> OcatraRoutes.find r "/ba")

  let suite =
    "ocatra_routes" >:::
      [
        "test_bind_and_find" >:: test_bind_and_find;
      ]
end

module OcatraStaticFileTest = struct
  let prefix = "ocatratest"

  let create_content suffix data =
    let (filepath, out_ch) = Filename.open_temp_file prefix suffix in
    output_string out_ch data;
    close_out out_ch;
    OcatraStaticFile.get_content filepath

  let test_text_html () =
    let data = "<html>
<head>
</head>
<body>
<p>hello world</p>
</body>
</html>" in
    let content = create_content ".html" data in
    assert_equal (Content.TextHtml data) @@ content

  let test_text_plain () =
    let data = "Hello world" in
    let content = create_content ".txt" data in
    assert_equal (Content.TextPlain data) @@ content

  let test_application_json () =
    let data = "{\"name\":\"komamitsu\",\"age\":99}" in
    let content = create_content ".json" data in
    assert_equal (Content.ApplicationJson data) @@ content

  let test_application_xml () =
    let data = "<note>
<to>Tove</to>
<from>Jani</from>
<heading>Reminder</heading>
<body>Don't forget me this weekend!</body>
</note>" in
    let content = create_content ".xml" data in
    assert_equal (Content.ApplicationXml data) @@ content

  let suite =
    "ocatra_static_file" >:::
      [
        "test_text_html" >:: test_text_html;
        "test_text_plain" >:: test_text_plain;
        "test_text_application_json" >:: test_application_json;
        "test_text_application_xml" >:: test_application_xml;
      ]
end

let _ =
  ignore (run_test_tt_main HttpResponseTest.suite);
  ignore (run_test_tt_main HttpRequestTest.suite);
  ignore (run_test_tt_main OcatraRoutesTest.suite);
  ignore (run_test_tt_main OcatraStaticFileTest.suite)

