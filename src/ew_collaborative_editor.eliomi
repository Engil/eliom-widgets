{server{

type t
type doc = {id : int; text : string;}

val new_document : string -> doc
val create : unit -> t
val init_and_register : t -> doc Eliom_reference.eref -> unit Lwt.t
val get_elt : t -> Html5_types.div Eliom_content.Html5.elt
}}
