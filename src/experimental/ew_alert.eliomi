{shared{
  open Eliom_content.Html5
  open Html5_types
}}

{shared{
  (** The type of the parent element. *)
  type parent' =
      [
        | body_content
      ]

  (** The type of the alert element. *)
  type element' =
      [
        | body_content
      ]

  (** The type of an html element. *)
  type 'a elt' = 'a Eliom_content.Html5.elt
}}

{client{
  module type T = sig
    include Ojw_alert_f.T
      with type 'a elt = 'a elt'
       and type parent = parent'
       and type element = element'
  end

  include Ojw_alert_f.T
    with type 'a elt = 'a elt'
     and type parent = parent'
     and type element = element'
}}

{client{
  type t' = t
}}

{server{
  type t'
}}

{shared{
  (** The type of the function used to generate the alert content. *)
  type alert_fun = t' -> element' elt'
}}