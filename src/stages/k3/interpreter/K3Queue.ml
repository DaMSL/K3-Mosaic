(* Specialized queues for the K3 Interpreter *)
open Util

module Q = Queue
module S = Stack

exception Empty

(* the implementation is a stack of queues *)
type 'a queue = ('a Q.t) S.t
type 'a t = {
  mutable increase_level : bool;
  q : 'a queue;
}

let create () =
  let s = S.create () in
  {increase_level=false; q=s}

let increase_level s = s.increase_level <- true

let push x s =
  if s.increase_level || S.is_empty s.q then
    let q = Q.create () in
    Q.push x q;
    S.push q s.q;
    (* reset the level increase flag *)
    if s.increase_level then s.increase_level <- false;
  else 
    let q = S.top s.q in
    Q.push x q

let is_empty s = S.is_empty s.q

let peek s =
  try
    let q = S.top s.q in
    Q.peek q
  with
  | Q.Empty | S.Empty -> raise Empty

let pop s =
  try
    let q = S.top s.q in
    let x = Q.pop q in
    (* clean up *)
    if Q.is_empty q then ignore(S.pop s.q);
    x
  with
  | Q.Empty | S.Empty -> raise Empty

  

