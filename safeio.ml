type channel = Closed | In of in_channel | Out of out_channel
type channels = {mutable c: channel; mutable n: channels option}
type ('a,'b) monad = channels -> 'b
type ('a,'b) handle = int
type ai    (* Permissions *)
type ao
type +'a active
type input = ai active
type output = ao active
type closed
type all_closed = <c: closed; n: all_closed>

let ch1 = 0
let ch2 = 1
let succ n = n+1

let rec nth ch n =
  if n = 0 then ch else
  let ch =
    match ch.n with
      Some ch -> ch
    | None ->
	let ch' = {c = Closed; n = None} in
	ch.n <- Some ch'; ch'
  in
  nth ch (n-1)

let rec length ch =
  match ch.n with None -> 1 | Some ch -> 1 + length ch

let close i ch =
  match (nth ch i).c with
    In ic  -> close_in ic
  | Out oc -> close_out oc
  | Closed -> ()

let run f =
  let ch = {c = Closed; n = None} in
  try f ch
  with exn ->
    for i = 1 to length ch do close i ch done; raise exn

let open_in i s ch =
  (nth ch i).c <- In (open_in s)

let open_out i s ch =
  (nth ch i).c <- Out (open_out s)

let output i c ch =
  match (nth ch i).c with
    Out oc -> output_char oc c
  | _ -> assert false

let input i ch =
  match (nth ch i).c with
    In ic -> begin try Some (input_char ic) with _ -> None end
  | _ -> assert false

let return x ch = x

let bind f1 f2 ch = f2 (f1 ch) ch

let ($) f1 f2 = bind f1 (fun () -> f2) ;;
