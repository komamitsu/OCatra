open OUnit
open String
open HttpCommon

module HttpRequestTest = struct
  open HttpRequest

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

    let inch = open_in filename in
    (f inch);
    close_in inch;
    Unix.unlink filename

  let test_parse_ok1 () =
    let header =
      "GET /hoge/foo/bar?name=zxcv&age=123 HTTP/1.1\r\n" ^
      "Host:google.com\r\n" ^
      "Cookie : sessionid=1qaz2wsx3edc4rf\r\n"
    in
    test_request header None
    (fun inch ->
      let req = parse_request inch in
      assert_equal HttpMethod.Get req.methd;
      assert_equal "/hoge/foo/bar" req.path;
      assert_equal "HTTP/1.1" req.version;
      assert_equal "zxcv" (HttpParam.find req.param "name");
      assert_equal "123" (HttpParam.find req.param "age");
      assert_equal "google.com" (HttpHeader.find req.header "Host");
      assert_equal "sessionid=1qaz2wsx3edc4rf" (HttpHeader.find req.header "Cookie");
      assert_equal HttpContent.None req.content
    )

  let test_parse_ok2 () =
    let header =
      "POST /hoge/foo/bar HTTP/1.1\r\n" ^
      "Host:google.com:8080\r\n" ^
      "Content-Type: application/x-www-form-urlencoded\r\n" ^
      "Cookie : sessionid=1qaz2wsx3edc4rf\r\n" in
    let body = Some "name=komamitsu&age=73&blood=x" in
    test_request header body
    (fun inch ->
      let req = parse_request inch in
      assert_equal HttpMethod.Post req.methd;
      assert_equal "/hoge/foo/bar" req.path;
      assert_equal "HTTP/1.1" req.version;
      assert_equal "google.com:8080" (HttpHeader.find req.header "Host");
      assert_equal "sessionid=1qaz2wsx3edc4rf" (HttpHeader.find req.header "Cookie");
      match req.content with
      | HttpContent.ApplicationXWwwFormUrlencoded params ->
          assert_equal 3 (HttpParam.length params);
          assert_equal "komamitsu" (HttpParam.find params "name");
          assert_equal "73" (HttpParam.find params "age");
          assert_equal "x" (HttpParam.find params "blood")
      | _ -> failwith "wrong content_type"
    )

  let test_parse_ng_comma_missing () =
    let header =
      "GET /hoge/foo/bar?name=zxcv&age=123 HTTP/1.1\r\n" ^
      "Host:google.com\r\n" ^
      "Cookie sessionid=1qaz2wsx3edc4rf"
    in
    test_request header None
    (fun inch ->
      let test () = parse_request inch in
      assert_raises (HttpError HttpStatus.BadRequest) test
    )

  let test_keepalive () =
    let header = HttpHeader.create 4 in
    assert_equal true @@ keepalive "HTTP/1.1" header;
    assert_equal false @@ keepalive "HTTP/1.0" header;
    HttpHeader.replace header "Connection" "close";
    assert_equal false @@ keepalive "HTTP/1.1" header;
    assert_equal false @@ keepalive "HTTP/1.0" header;
    HttpHeader.replace header "Connection" "Keep-Alive";
    assert_equal true @@ keepalive "HTTP/1.1" header;
    assert_equal true @@ keepalive "HTTP/1.0" header
 
  let suite = 
    "http_request" >:::
      [
        "test_parse_ok1" >:: test_parse_ok1;
        "test_parse_ok2" >:: test_parse_ok2;
        "test_parse_ng_comma_missing" >:: test_parse_ng_comma_missing;
        "test_keepalive" >:: test_keepalive;
      ]
end

module HttpResponseTest = struct
  open HttpResponse

  let test_response ?(status=HttpStatus.OK) ?(header=HttpHeader.create 0) content f =
    let filename = "test_response.txt" in
    let och = open_out filename in
    response och @@ create_response ~status:status ~header:header content ();
    close_out och;
    let inch = open_in filename in
    (f inch);
    close_in inch;
    Unix.unlink filename

  let test_ok1 () =
    test_response HttpContent.None (fun inch ->
      assert_equal "HTTP/1.1 200 OK\r" @@ input_line inch
    )

  let test_ok2 () =
    let body = "Hello, World" in
    test_response (HttpContent.TextPlain body)
      (fun inch ->
        assert_equal "HTTP/1.1 200 OK\r" @@ input_line inch;
        assert_equal "Content-Type: text/plain\r" @@ input_line inch;
        assert_equal ("Content-Length: " ^ string_of_int (length body) ^ "\r") @@ input_line inch;
        assert_equal "\r" @@ input_line inch;
        assert_equal body @@ input_line inch
      )

  let test_ng_not_found () =
    test_response ~status:HttpStatus.NotFound HttpContent.None (fun inch ->
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

let _ =
  ignore (run_test_tt_main HttpResponseTest.suite);
  ignore (run_test_tt_main HttpRequestTest.suite)

