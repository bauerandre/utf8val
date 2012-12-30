(** Unicode code point filters *)

(**
   Functions in this module check whether code points are within valid ranges
   without using large lookup tables.
*)

val is_allowed : int -> bool
  (** Whether the given code point is allowed in publicly-exchanged documents.
      This excludes non-characters and private use areas. *)

val is_allowed_and_assigned : int -> bool
  (** Whether the given code point is allowed and was assigned
      by the Unicode consortium.
      More code points may be assigned in the future. *)
