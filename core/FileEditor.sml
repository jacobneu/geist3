infix |> ***
fun x |> f = f x
fun id x = x
fun (f *** g) (x,y) = (f x, g y)
fun pr1 (x,y) = x
fun pr2 (x,y) = y

structure FileCommand =
struct

    type content = string

    datatype domAddress =
          Content
        | History
        | Header
        | Info

    datatype command = 
          SKIP
        | CLEAN
        | GET of domAddress
        | SET of domAddress * content
        | APPEND of domAddress * content
                    

end

(* Eventually, package this as a utility for use by various sml scripts *)
structure Reader = 
struct
    local
      val clean = 
        String.implode o (List.filter (Fn.curry op<> #"\n")) o String.explode
    in
      fun toList (infile:string):string list = 
        let
          val ins = TextIO.openIn infile
          fun loop ins =
            case TextIO.inputLine ins of
              SOME line => line :: loop ins
            | NONE      => []
         in
           map clean (loop ins before TextIO.closeIn ins)
         end
    end
end


open FileCommand

functor FileEditor(val path : string) :>
sig

  type gpf
  val read : unit -> gpf
  val perform : command -> unit

end =
struct

  (* TODO: Parametrize and store elsewhere, add utilities for extending *)
    datatype processMode = 
        NORMAL
      | $$
      | HH
      | %%
  fun pMtoString M = case M of
      NORMAL => "NORMAL"
      | $$ => "$$"
      | HH => "HH"
      | %% => "%%"
  fun pMfromString s = case s of
      "NORMAL" => NORMAL
      | "$$" => $$
      | "HH" => HH
      | "%%" => %%
      | _ => NORMAL (* TODO: Make error *)

  fun pMfromBString bs = case bs of
      "[NORMAL]" => NORMAL
      | "[$$]" => $$
      | "[HH]" => HH
      | "[%%]" => %%
      | _ => NORMAL (* TODO: Make error *)


  datatype linetype =
      Blank
    | Plaintext of string
    | MetaBlock of processMode (* INVARIANT: not 'NORMAL' *)
    | MetaBlank

  val Mode = ref NORMAL

  (* If !Mode is NORMAL, then flip MM sets Mode to MM.
   *  If !Mode isnt NORMAL, then flip MM sets Mode to NORMAL *)
  val flipVerbose = ref false 
  fun flip MM =
    let
      val _ = if !flipVerbose
              then print (
                "FLIP: " ^ (pMtoString (!Mode)) ^ " to " ^ (pMtoString MM) ^ "\n"
              )
              else ()
    in
      Mode := (case !Mode of 
                NORMAL => MM
              | _ => NORMAL) (* TODO: Add warning if !Mode<>MM *)
    end

  structure GPF : 
  sig
    type gpf
    val appendContent : gpf -> linetype -> gpf
    val appendHead : gpf -> string*string list -> gpf
    val Nothing : gpf
    val getContent : gpf -> linetype list
    val getHead : gpf -> (string * string list) list
  end
  = 
  struct
    
    (* INVARIANT: content lines are stored in reverse order *)
    type gpf = linetype list * (string * string list) list

    (* Basic utility functions *)

    (* appendContent : gpf * linetype -> gpf *)
    fun appendContent (G,H) foo = (foo :: G,H)
    fun appendHead (G,H) foo = (G,foo::H)
    val Nothing : gpf = ([],[])
    fun getContent ((L,_):gpf):linetype list = List.rev L
    fun getHead (_,H) = List.rev H

  end
  open GPF

    val spaceSplit = fn s => (s,
    let val ls = String.tokens Char.isSpace s
    in
      (* print ((String.concatWith ";" ls)^"\n"); *)
       ls
    end)
    val unSpaceSplit = String.concatWith " "

    (* process : (string * string list) -> gpf -> gpf *)

    fun process (_,[]) acc = (case !Mode of NORMAL => appendContent acc Blank
                                         | _ => appendContent acc MetaBlank)
      | process (_, [MMBstr as ("[$$]"|"[HH]"|"[%%]")]) acc =
            let 
               val MM = pMfromBString MMBstr
               val _ = flip MM
            in
             appendContent acc (MetaBlock MM)
            end
      | process (_,"[$$]"::rest) acc = (flip $$; 
                                        process
                                          (unSpaceSplit rest,rest)
                                          (appendContent acc (MetaBlock $$))
                                       )
      | process (_,"[HH]"::rest) acc = (flip HH; 
                                        process
                                          (unSpaceSplit rest,rest) 
                                          (appendContent acc (MetaBlock HH))
                                       )
      | process (_,"[%%]"::rest) acc = (flip %%; 
                                        process
                                          (unSpaceSplit rest,rest) 
                                          (appendContent acc (MetaBlock %%))
                                       )
      | process (s,tokens as t0::rest) acc = 
            (case !Mode of
                 NORMAL => appendContent acc (Plaintext s)
              |  $$ => appendHead acc (t0,rest)
              |  HH => process (s,rest) acc (* TODO: Make it actually store the info *)
              |  %% => process (s,rest) acc (* TODO: Make it actually store the info *)
              (*|  _ => Plaintext s :: acc*)
             )
    (* fun read () = List.rev(List.foldMapl (Fn.uncurry process) spaceSplit []
      (Reader.toList path))*)

    fun read () = 
          path  |> Reader.toList                     (* string list *)
                |> map spaceSplit                    (* string * (string list) list*)
                |> foldl (Fn.uncurry process) Nothing


  datatype printFun = JustContent | All | Debug

  fun printCont pf =
    let
      fun echo Debug (Plaintext s) = print ("[TEXTLINE:"^s^"]")
        | echo _ (Plaintext s) = print(s^"\n")

        | echo Debug Blank = print("[BLANKLINE]") 
        | echo _ Blank = print("\n")

        | echo Debug MetaBlank = print("[METABLANK]")
        | echo _ MetaBlank = print("") 

        | echo Debug (MetaBlock M) = print("[META " ^ (pMtoString M) ^ "]") 
        | echo _ (MetaBlock M) = ()
    in
      ignore o (map (echo pf)) o getContent
    end
  fun printHead G =
    let
      val indent = "    "
      fun echo (tok,cont) = 
        print(indent ^ tok ^ " " ^ (String.concatWith " " cont) ^ "\n")
      fun printSS _ = print "[$$]\n"
    in
      (printSS o 
      (fn _ => map echo (getHead G)) o 
      printSS) ()
    end
                            


  fun perform SKIP = ()
    | perform CLEAN = 
        let 
          val G = read()
        in
          (printHead G;
          printCont All G)
        end
end


