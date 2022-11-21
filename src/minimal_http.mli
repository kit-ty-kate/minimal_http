type t

val server_handler :
  error_handler:(t -> unit) ->
  request_handler:(t -> unit) ->
  ('a, 'b) Alpn.server_handler

val meth : t -> Httpaf.Method.t
val target : t -> string
val response : t -> status:Httpaf.Status.t -> string -> unit
