(** UTF-8 Validator *)

val is_utf8 : string -> bool
  (** Return true if and only if the given string is follows a
      valid UTF-8 encoding.
      This function does not check whether the code points are valid
      or representable Unicode characters. *)

exception Malformed of int option * string
(** Exception raised by the [fold_left] and [iter].
    The first argument is the position where the error occurred,
    position in the string which comes as the second argument. *)

val fold_left : ('a -> int -> 'a) -> 'a -> string -> 'a
  (** Like [List.fold_left], but iterate over the Unicode code points.
      @raise [Malformed] if the input is malformed UTF-8 *)

val iter : (int -> unit) -> string -> unit
  (** Like [List.iter], but iterate over the Unicode code points.
      @raise [Malformed] if the input is malformed UTF-8 *)

val is_assigned_unicode : string -> bool
  (** Check whether the string is valid UTF-8 and contains Unicode code points
      assigned by the Unicode consortium
      (assigned ranges within planes 0, 1, 2 and 14).

      This filter allows any string that represents valid Unicode "today", but
      exclude ranges that are not yet assigned.
  *)

val is_public_unicode : string -> bool
  (** Check whether the string is valid UTF-8 and contains Unicode code points
      that are not in private use areas
      (all code points except private use areas).

      This filter allows any string that might represent valid Unicode
      now or in the future.
  *)
