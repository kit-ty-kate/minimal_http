module Headers = struct
  module Map = Map.Make (String)

  type t = string Map.t

  let empty = Map.empty
  let add_content_type map x = Map.add "content-type" x map
end

type t = {
  meth : Httpaf.Method.t;
  target : string;
  response : status:Httpaf.Status.t -> headers:Headers.t -> body:string -> unit;
}

let http_1_1_request_handler ~request_handler reqd =
  let request = Httpaf.Reqd.request reqd in
  let self = {
    meth = request.Httpaf.Request.meth;
    target = request.Httpaf.Request.target;
    response = (fun ~status ~headers ~body ->
      let headers = Httpaf.Headers.of_list (Headers.Map.bindings headers) in
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
      let headers = H2.Headers.of_list (Headers.Map.bindings headers) in
      let response = H2.Response.create ~headers (status :> H2.Status.t) in
      H2.Reqd.respond_with_string reqd response body
    );
  } in
  request_handler self

let alpn_request_handler
  : type reqd headers request response ro wo.
    request_handler:_ -> _ -> _ -> reqd -> (reqd, headers, request, response, ro, wo) Alpn.protocol -> unit
  = fun ~request_handler _flow _edn reqd -> function
  | Alpn.HTTP_1_1 _ -> http_1_1_request_handler ~request_handler reqd
  | Alpn.H2 _ -> http_2_0_request_handler ~request_handler reqd


let server_handler ~error_handler:_ ~request_handler = {
  Alpn.
  error = (fun _edn _protocol ?request:_ _error _respond -> ());
  request = (fun flow edn reqd protocol -> alpn_request_handler ~request_handler flow edn reqd protocol);
}

let meth {meth; _} = meth
let target {target; _} = target
let response {response; _} = response
