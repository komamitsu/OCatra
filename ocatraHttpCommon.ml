open Unix
open Printf

let (@@) f x = f x
let ($) f g x = f (g x)

let p s = print_endline s;
  Pervasives.flush Pervasives.stdout

module StatusCategory = struct
  type t = IntermediateStatus | SuccessfulResponse | Redirects | RequestErrors | ServerErrors
end

module Status = struct
  open StatusCategory

  type t =
    | Continue
    | SwitchingProtocols
    | OK
    | Created
    | Accepted
    | NonAuthoritativeInformation
    | NoContent
    | ResetContent
    | PartialContent
    | MultipleChoices
    | MovedPermanently
    | MovedTemporarily
    | SeeOther
    | NotModified
    | UseProxy
    | Unused
    | TemporaryRedirect
    | BadRequest
    | Unauthorized
    | PaymentRequired
    | Forbidden
    | NotFound
    | MethodNotAllowed
    | NotAcceptable
    | ProxyAuthenticationRequired
    | RequestTimeOut
    | Conflict
    | Gone
    | LengthRequired
    | PreconditionFailed
    | RequestEntityTooLarge
    | RequestURITooLarge
    | UnsupportedMediaType
    | RequestedRangeNotSatisfiable
    | ExpectationFailed
    | InternalServerError
    | NotImplemented
    | BadGateway
    | ServiceUnavailable
    | GatewayTimeOut
    | HTTPVersionNotSupported

  let category_of_status = function
    | Continue -> IntermediateStatus
    | SwitchingProtocols -> IntermediateStatus
    | OK -> SuccessfulResponse
    | Created -> SuccessfulResponse
    | Accepted -> SuccessfulResponse
    | NonAuthoritativeInformation -> SuccessfulResponse
    | NoContent -> SuccessfulResponse
    | ResetContent -> SuccessfulResponse
    | PartialContent -> SuccessfulResponse
    | MultipleChoices -> Redirects
    | MovedPermanently -> Redirects
    | MovedTemporarily -> Redirects
    | SeeOther -> Redirects
    | NotModified -> Redirects
    | UseProxy -> Redirects
    | Unused -> Redirects
    | TemporaryRedirect -> Redirects
    | BadRequest -> RequestErrors
    | Unauthorized -> RequestErrors
    | PaymentRequired -> RequestErrors
    | Forbidden -> RequestErrors
    | NotFound -> RequestErrors
    | MethodNotAllowed -> RequestErrors
    | NotAcceptable -> RequestErrors
    | ProxyAuthenticationRequired -> RequestErrors
    | RequestTimeOut -> RequestErrors
    | Conflict -> RequestErrors
    | Gone -> RequestErrors
    | LengthRequired -> RequestErrors
    | PreconditionFailed -> RequestErrors
    | RequestEntityTooLarge -> RequestErrors
    | RequestURITooLarge -> RequestErrors
    | UnsupportedMediaType -> RequestErrors
    | RequestedRangeNotSatisfiable -> RequestErrors
    | ExpectationFailed -> RequestErrors
    | InternalServerError -> ServerErrors
    | NotImplemented -> ServerErrors
    | BadGateway -> ServerErrors
    | ServiceUnavailable -> ServerErrors
    | GatewayTimeOut -> ServerErrors
    | HTTPVersionNotSupported -> ServerErrors

  let string_of_status = function
    | Continue -> "100 Continue"
    | SwitchingProtocols -> "101 Switching Protocols"
    | OK -> "200 OK"
    | Created -> "201 Created"
    | Accepted -> "202 Accepted"
    | NonAuthoritativeInformation -> "203 Non-Authoritative Information"
    | NoContent -> "204 No Content"
    | ResetContent -> "205 Reset Content"
    | PartialContent -> "206 Partial Content"
    | MultipleChoices -> "300 Multiple Choices"
    | MovedPermanently -> "301 Moved Permanently"
    | MovedTemporarily -> "302 Moved Temporarily"
    | SeeOther -> "303 See Other"
    | NotModified -> "304 Not Modified"
    | UseProxy -> "305 Use Proxy"
    | Unused -> "306 Unused"
    | TemporaryRedirect -> "307 Temporary Redirect"
    | BadRequest -> "400 Bad Request"
    | Unauthorized -> "401 Unauthorized"
    | PaymentRequired -> "402 Payment Required"
    | Forbidden -> "403 Forbidden"
    | NotFound -> "404 Not Found"
    | MethodNotAllowed -> "405 Method Not Allowed"
    | NotAcceptable -> "406 Not Acceptable"
    | ProxyAuthenticationRequired -> "407 Proxy Authentication Required"
    | RequestTimeOut -> "408 Request TimeOut"
    | Conflict -> "409 Conflict"
    | Gone -> "410 Gone"
    | LengthRequired -> "411 Length Required"
    | PreconditionFailed -> "412 Precondition Failed"
    | RequestEntityTooLarge -> "413 Request Entity Too Large"
    | RequestURITooLarge -> "414 Request-URI Too Large"
    | UnsupportedMediaType -> "415 Unsupported Media Type"
    | RequestedRangeNotSatisfiable -> "416 Requested Range Not Satisfiable"
    | ExpectationFailed -> "417 Expectation Failed"
    | InternalServerError -> "500 Internal Server Error"
    | NotImplemented -> "501 Not Implemented"
    | BadGateway -> "502 Bad Gateway"
    | ServiceUnavailable -> "503 Service Unavailable"
    | GatewayTimeOut -> "504 Gateway Time-out"
    | HTTPVersionNotSupported -> "505 HTTP Version Not Supported"
end

exception HttpError of Status.t

module Method = struct
  open Status

  type t = Get | Post | Put | Delete

  let method_of_string = function
    | "GET" -> Get
    | "POST" -> Post
    | "PUT" -> Put 
    | "DELETE" -> Delete
    | _ -> raise (HttpError BadRequest)

  let string_of_method = function
    | Get -> "GET"
    | Post -> "POST"
    | Put -> "PUT"
    | Delete -> "DELETE"
end

module Header = Hashtbl.Make(
  struct
    type t = string
    let equal = (=)
    let hash = Hashtbl.hash
  end
)

module Param = Hashtbl.Make(
  struct
    type t = string
    let equal = (=)
    let hash = Hashtbl.hash
  end
)

module Util = struct
  let re_for_query_sep = Str.regexp "&"

  let re_for_query_kv = Str.regexp "="

  let parse_kv_joined_by_eq replacef tbl s =
    let qs = Str.split re_for_query_sep s in
    List.fold_left
      (fun tbl kv ->
        let kvs = Str.split re_for_query_kv kv in
        if List.length kvs == 2 then begin
          replacef tbl (List.nth kvs 0) (List.nth kvs 1);
          tbl
        end
        else raise (HttpError Status.BadRequest)) tbl qs

  let log s = 
    (*
      print_endline s
    *)
    ()

  let http_error st = raise (HttpError st)
end

module Content = struct
  open Util
  open Status

  type t =
    | None
    | TextPlain of string
    | TextHtml of string
    | ApplicationOctetStream of string
    | ApplicationXWwwFormUrlencoded of string Param.t
    | ApplicationXml of string
    | ApplicationJson of string

  let read_as_string content_length inch =
    let line = String.create content_length in
    let read_len = input inch line 0 content_length in
    if read_len = content_length then line
    else raise (HttpError Status.BadRequest)

  let read_as_kv_joined_by_eq content_length inch =
    let line = read_as_string content_length inch in
    Util.parse_kv_joined_by_eq Param.replace (Param.create 6) line

  let create content_length inch = function
    | "text/plain" -> TextPlain (read_as_string content_length inch)
    | "text/html" -> TextHtml (read_as_string content_length inch)
    | "application/octet-stream" ->
        ApplicationOctetStream (read_as_string content_length inch)
    | "application/x-www-form-urlencoded" ->
        ApplicationXWwwFormUrlencoded (read_as_kv_joined_by_eq content_length inch)
    | "application/xml" -> ApplicationXml (read_as_string content_length inch)
    | "application/json" -> ApplicationJson (read_as_string content_length inch)
    | _ -> raise (HttpError BadRequest)

  let string_of_content_type = function
    | TextPlain _ -> "text/plain"
    | TextHtml _ -> "text/html"
    | ApplicationOctetStream _ -> "application/octet-stream"
    | ApplicationXWwwFormUrlencoded _ -> "application/x-www-form-urlencoded"
    | ApplicationXml _ -> "application/xml"
    | ApplicationJson _ -> "application/json"
    | _ -> http_error BadRequest

  let string_of_content_body = function
    | TextPlain s -> s
    | TextHtml s -> s
    | ApplicationOctetStream s -> s
    | ApplicationXWwwFormUrlencoded tbl ->
        Util.http_error Status.InternalServerError
    | ApplicationXml s -> s
    | ApplicationJson s -> s
    | _ -> http_error BadRequest
end


