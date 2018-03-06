# safeio

An encoding of file handles ensuring linear use.

This code was originally posted to the caml-list on 2006-05-11:
[original post](http://caml.inria.fr/pub/ml-archives/caml-list/2006/05/af47dc2040740cc1e546bd353fa18115.en.html)

Here is a copy of the original post.
This repository contains a more refined version written shortly after.

Jacques Garrigue


<pre>
From: Dan Grossman

> A monadic approach (where each
> operation would return a "new" file) or linearity appears necessary for
> the latter.

And you can perfectly encode the monadic approach in ocaml.
In our case, we need the type of the monad to keep information about
open and closed files.
I include such a solution, which ensures the safety of file accesses,
at the end of this post. Note that file handles are indexed
statically, but you can use as many as you wish.

It should be safe with references (there is no file handle that you
can keep around, everything stays in the monad.) But beware of fancy
extensions, like continuations, that would allow you to capture your
environement, included files that were open when you created the
continuation...

From: Geoffrey Alan Washburn

> 	The only "solution" that I've seen that seems to actually solve the 
> "problem" is to write a small interpreted language for performing I/O, 
> but that is a bit heavy weight for general use.

I see indeed no other easy option if you have continuations in the
language.

Jacques

(* safeio.mli *)
type ('a,'b) monad    (* A monad, with action 'a, returning 'b *)
type ('a,'b) handle   (* A file handle to be used/modified *)
type input = [`In]    (* Permissions *)
type output = [`Out]
type active = [`In|`Out]
type closed = [`Closed]
type all_closed = <c: closed; n: all_closed> (* All files closed *)

val ch1 : (<c:'a;n:'b> -> <c:'c;n:'b>, 'a -> 'c) handle
val succ : ('a -> 'b, 'c) handle -> (<c:'d;n:'a> -> <c:'d;n:'b>, 'c) handle
val ch2 : (* shorthand for [succ ch1] *)
    (<c:'a; n: <c:'b; n:'c> > -> <c:'a; n: <c:'d; n:'c> >, 'b -> 'd) handle

val run : (all_closed -> all_closed, 'a) monad -> 'a

val open_in : ('a, closed -> input) handle -> string -> ('a, unit) monad
val open_out : ('a, closed -> output) handle -> string -> ('a, unit) monad
val close : ('a, [< active] -> closed) handle -> ('a, unit) monad
val input : ('a, input -> input) handle -> ('a, char option) monad
val output : ('a, output -> output) handle -> char -> ('a, unit) monad

val return : 'a -> ('b -> 'b, 'a) monad
val bind :
  ('a -> 'b, 'd) monad -> ('d -> ('b -> 'c, 'e) monad) -> ('a -> 'c, 'e) monad
val ( $ ) :
  ('a -> 'b, unit) monad -> ('b -> 'c, 'd) monad -> ('a -> 'c, 'd) monad

(* safeio.ml *)
type channel = Closed | In of in_channel | Out of out_channel
type channels = {mutable c: channel; mutable n: channels option}
type ('a,'b) monad = channels -> 'b
type ('a,'b) handle = int
(* Implementation left as exercise... *)

(* Example of use *)
open Safeio ;;

let rec copy2 () =
  bind (input ch1)
    (function None -> return ()
    | Some c -> bind (output ch2 c) copy2);;
val copy2 : unit ->
  (< c : input; n : < c : output; n : 'a > > ->
   < c : input; n : < c : output; n : 'a > >, unit) monad

let copy_file f1 f2 =
  open_in ch1 f1 $
  open_out ch2 f2 $
  copy2 () $
  close ch1 $
  close ch2 ;;

run (copy_file "a" "b") ;;
</pre>