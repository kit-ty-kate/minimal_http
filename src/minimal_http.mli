type t
type error

module Headers : sig
  type t

  val empty : t
  val add_content_type : t -> string -> t
end

val server_handler :
  error_handler:(error -> unit) ->
  request_handler:(t -> unit) ->
  ('a, 'b) Alpn.server_handler

val meth : t -> Httpaf.Method.t
val target : t -> string
val response : t -> status:Httpaf.Status.t -> headers:Headers.t -> body:string -> unit

val error_kind : error -> [`Bad_gateway | `Bad_request | `Exn of exn | `Internal_server_error]
val error_response : error -> status:Httpaf.Status.t -> headers:Headers.t -> body:string -> unit
