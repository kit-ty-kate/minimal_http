type t

module Headers : sig
  type t

  val empty : t
  val add_content_type : t -> string -> t
end

val server_handler :
  error_handler:(t -> unit) ->
  request_handler:(t -> unit) ->
  ('a, 'b) Alpn.server_handler

val meth : t -> Httpaf.Method.t
val target : t -> string
val response : t -> status:Httpaf.Status.t -> headers:Headers.t -> body:string -> unit
