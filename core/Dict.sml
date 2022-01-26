signature EQ =
sig
  type t
  (* INVARIANT: equal is a reasonable notion of equality *)
  val equal : t -> t -> bool
end

signature ORD =
sig
  type t

  (* INVARIANT: equal is a comparison function *)
  val cmp : t * t -> order
end





(* FRAGMENT 3 *)
signature DICT =
sig
  structure Key : EQ

  type 'a entry = Key.t * 'a
  type 'a dict

  val empty : 'a dict

  exception ExistingEntry  
  val insert : 'a entry * 'a dict -> 'a dict
  val overwrite : 'a entry * 'a dict -> 'a dict
  val updateCmb : 'a dict -> 'b entry -> ('b * 'a -> 'a) -> ('b -> 'a ) -> 'a dict
  val lookup : 'a dict -> Key.t -> 'a option
  val lookupDefault : 'a dict -> Key.t -> 'a -> 'a
  val keys : 'a dict -> Key.t list
end
(* END 3 *)
(* 15-150, Summer 2021                                          *)
(* Jacob Neumann, based on code by
 * Michael Erdmann & Frank Pfenning                             *)

(************************************************************************)

functor cmpEqual (K : ORD):EQ =
struct
  type t = K.t
  fun equal x y = K.cmp(x,y) = EQUAL
end
(* FRAGMENT 4 *)
structure StringOrd : ORD =
struct
  type t = string
  val cmp = String.compare
end

functor MkDict (KeyOrd : ORD) :> DICT 
  where type Key.t = KeyOrd.t =
struct
  structure Key = cmpEqual(KeyOrd)
  type 'a entry = Key.t * 'a
(* END 4 *)
  datatype 'a dict =
    Empty	
  | Red of 'a dict * 'a entry * 'a dict
  | Black of 'a dict * 'a entry * 'a dict

  fun keys Empty = []
    | keys (Red(L,(k,v),R)) = (keys L)@(k::keys R)
    | keys (Black(L,(k,v),R)) = (keys L)@(k::keys R)

  val empty = Empty

  fun lookup d k' =
    let
      fun lk (Empty) = NONE
        | lk (Red tree) = lk' tree
        | lk (Black tree) = lk' tree
      and lk' (L, (k,v), R) =
	    (case KeyOrd.cmp(k',k)
	       of EQUAL => SOME(v)
	        | LESS => lk L
            | GREATER => lk R)
    in
      lk d
    end
  fun lookupDefault d k default = 
     case lookup d k of
          (SOME x) => x
        | NONE => default
  fun lookupCont d k sc fc = 
     case lookup d k of
          (SOME x) => sc x
        | NONE => fc ()

  fun restoreLeft(Black(Red(Red(tlll, ell, tllr), el, tlr), e, tr)) =
        Red(Black(tlll, ell, tllr), el, Black(tlr, e, tr))
    | restoreLeft(Black(Red(tlll, ell, Red(tllr, el, tlr)), e, tr)) =
        Red(Black(tlll, ell, tllr), el, Black(tlr, e, tr))
    | restoreLeft D = D

  fun restoreRight(Black(tl, e, Red(trl, er, Red(trrl, z, trrr)))) =
        Red(Black(tl, e, trl), er, Black(trrl, z, trrr))
    | restoreRight(Black(tl, e, Red(Red(trl, er, trrl), z, trrr))) =
        Red(Black(tl, e, trl), er, Black(trrl, z, trrr))
    | restoreRight D = D
 
  exception ExistingEntry

  fun overwrite ((k',v'), D) =
    let
      fun helpero Empty = Red(Empty,(k',v'),Empty)
        | helpero (Red(L,(k,v),R)) =
           (case KeyOrd.cmp(k',k) of
             LESS => Red(helpero L,(k,v),R)
           | EQUAL => Red(L,(k',v'),R)
           | GREATER => Red(L,(k,v),helpero R))
        | helpero (Black(L,(k,v),R)) =
           case KeyOrd.cmp(k',k) of
             LESS => restoreLeft(Black(helpero L,(k,v),R))
           | EQUAL => Black(L,(k',v'),R)
           | GREATER => restoreRight(Black(L,(k,v),helpero R))
    in
      case helpero D of 
       Red (t as (Red _, _, _)) => Black t
	 | Red (t as (_, _, Red _)) => Black t
	 | D' => D'
    end

  fun updateCmb D (k,v') cmb pack =
    overwrite ((k, lookupCont D k (fn v=>cmb(v',v)) (fn ()=>pack v')), D)

  fun insert ((k',v'), D) =
    let
      fun helperi Empty = Red(Empty,(k',v'),Empty)
        | helperi (Red(L,(k,v),R)) =
           (case KeyOrd.cmp(k',k) of
             LESS => Red(helperi L,(k,v),R)
           | EQUAL => raise ExistingEntry
           | GREATER => Red(L,(k,v),helperi R))
        | helperi (Black(L,(k,v),R)) =
           case KeyOrd.cmp(k',k) of
             LESS => restoreLeft(Black(helperi L,(k,v),R))
           | EQUAL => raise ExistingEntry
           | GREATER => restoreRight(Black(L,(k,v),helperi R))
    in
      case helperi D of 
       Red (t as (Red _, _, _)) => Black t
	 | Red (t as (_, _, Red _)) => Black t
	 | D' => D'
    end
end 


