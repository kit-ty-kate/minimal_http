module Headers : sig
  type t

  val empty : t
  val add_content_type : t -> string -> t

  val to_list : body:string -> t -> (string * string) list
end = struct
  module Map = Map.Make (String)

  type t = string Map.t

  let empty = Map.empty
  let add_content_type map x = Map.add "content-type" x map

  let to_list ~body map =
    let map = Map.add "content-length" (string_of_int (String.length body)) map in
    Map.bindings map
end

type response = status:Httpaf.Status.t -> headers:Headers.t -> body:string -> unit

type t = {
  meth : Httpaf.Method.t;
  target : string;
  response : response;
}

type error = {
  kind : [`Bad_gateway | `Bad_request | `Exn of exn | `Internal_server_error];
  response : response;
}

let http_1_1_request_handler ~request_handler reqd =
  let request = Httpaf.Reqd.request reqd in
  let self = {
    meth = request.Httpaf.Request.meth;
    target = request.Httpaf.Request.target;
    response = (fun ~status ~headers ~body ->
      let headers = Httpaf.Headers.of_list (Headers.to_list ~body headers) in
      let response = Httpaf.Response.create ~headers status in
      Httpaf.Reqd.respond_with_string reqd response body
    );
  } in
  request_handler self

let http_2_0_request_handler ~request_handler reqd =
  let request = H2.Reqd.request reqd in
  let self = {
    meth = request.H2.Request.meth;
    target = request.H2.Request.target;
    response = (fun ~status ~headers ~body ->
      let headers = H2.Headers.of_list (Headers.to_list ~body headers) in
      let response = H2.Response.create ~headers (status :> H2.Status.t) in
      let writer = H2.Reqd.respond_with_streaming ~flush_headers_immediately:true reqd response in
      H2.Body.Writer.write_string writer body;
      H2.Body.Writer.close writer;
    );
  } in
  request_handler self

let alpn_request_handler
  : type reqd headers request response ro wo.
    request_handler:_ -> _ -> _ -> reqd -> (reqd, headers, request, response, ro, wo) Alpn.protocol -> unit
  = fun ~request_handler _flow _edn reqd -> function
  | Alpn.HTTP_1_1 _ -> http_1_1_request_handler ~request_handler reqd
  | Alpn.H2 _ -> http_2_0_request_handler ~request_handler reqd

let http_1_1_error_handler ~error_handler _edn ?request:_ error respond =
  let self = {
    kind = error;
    response = (fun ~status:_ ~headers:_ ~body:_ ->
      let resp = respond Httpaf.Headers.empty in
      Httpaf.Body.write_string resp "Error handled";
      Httpaf.Body.flush resp (fun () -> Httpaf.Body.close_writer resp)
    );
  } in
  error_handler self

let http_2_0_error_handler ~error_handler _edn ?request:_ error respond =
  let self = {
    kind = error;
    response = (fun ~status:_ ~headers:_ ~body:_ ->
      let resp = respond H2.Headers.empty in
      H2.Body.Writer.write_string resp "Error handled";
      H2.Body.Writer.flush resp (fun () -> H2.Body.Writer.close resp)
    );
  } in
  error_handler self

let alpn_error_handler
  : type reqd headers request response ro wo.
    error_handler:_ -> _ -> ?request:request -> _ -> (headers -> wo) ->
    (reqd, headers, request, response, ro, wo) Alpn.protocol -> unit
  = fun ~error_handler edn ?request error respond -> function
  | Alpn.HTTP_1_1 _ -> http_1_1_error_handler ~error_handler edn ?request error respond
  | Alpn.H2 _ -> http_2_0_error_handler ~error_handler edn ?request error respond

let server_handler ~error_handler ~request_handler = {
  Alpn.
  error = (fun edn protocol ?request error respond -> alpn_error_handler ~error_handler edn ?request error respond protocol);
  request = (fun flow edn reqd protocol -> alpn_request_handler ~request_handler flow edn reqd protocol);
}

let meth {meth; _} = meth
let target {target; _} = target
let response {response; meth = _; _} = response

let error_kind {kind; _} = kind
let error_response {response; kind = _} = response
