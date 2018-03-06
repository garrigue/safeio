type (+'a,+'b) monad    (* A monad, with action 'a, returning 'b *)
type (+'a,+'b) handle   (* A file handle to be used/modified *)
type ai    (* Permissions *)
type ao
type +'a active
type input = ai active
type output = ao active
type closed
type all_closed = <c: closed; n: all_closed> (* All files closed *)

(* Files are indexed statically, but you can use as many as you want *)
val ch1 : (<c:'a;n:'b> * <c:'c;n:'b>, 'a * 'c) handle
val succ : ('a * 'b, 'c) handle -> (<c:'d;n:'a> * <c:'d;n:'b>, 'c) handle
val ch2 : (* shorthand for [succ ch1] *)
    (<c:'a; n: <c:'b; n:'c> > * <c:'a; n: <c:'d; n:'c> >, 'b * 'd) handle

val run : (all_closed * all_closed, 'a) monad -> 'a

val open_in : ('a, closed * input) handle -> string -> ('a, unit) monad
val open_out : ('a, closed * output) handle -> string -> ('a, unit) monad
val close : ('a, 'b active * closed) handle -> ('a, unit) monad
val input : ('a, input * input) handle -> ('a, char option) monad
val output : ('a, output * output) handle -> char -> ('a, unit) monad

val return : 'a -> ('b * 'b, 'a) monad
val bind :
  ('a * 'b, 'd) monad -> ('d -> ('b * 'c, 'e) monad) -> ('a * 'c, 'e) monad
val ( $ ) :
  ('a * 'b, unit) monad -> ('b * 'c, 'd) monad -> ('a * 'c, 'd) monad


(*
open Safeio ;;

let rec copy2 () =
  bind (input ch1)
    (function None -> return ()
    | Some c -> bind (output ch2 c) copy2);;

let copy_file f1 f2 =
  open_in ch1 f1 $
  open_out ch2 f2 $
  copy2 () $
 close ch1 $
  close ch2 ;;
*)
