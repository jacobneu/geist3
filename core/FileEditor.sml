infix |> ***
fun x |> f = f x
fun id x = x
fun (f *** g) (x,y) = (f x, g y)
fun pr1 (x,y) = x
fun pr2 (x,y) = y

structure FileCommand =
struct

    type content = string

    datatype contentType = 
        PLAIN
      | $$
      | HH
      | %%
  fun pMtoString M = case M of
      PLAIN => "PLAIN"
      | $$ => "$$"
      | HH => "HH"
      | %% => "%%"
  fun pMfromString s = case s of
      "PLAIN" => PLAIN
      | "$$" => $$
      | "HH" => HH
      | "%%" => %%
      | _ => PLAIN (* TODO: Make error *)

  fun pMfromBString bs = case bs of
      "[PLAIN]" => PLAIN
      | "[$$]" => $$
      | "[HH]" => HH
      | "[%%]" => %%
      | _ => PLAIN (* TODO: Make error *)


    val frontMeta = [$$,%%]
    val backMeta = [HH]
    val allMeta = frontMeta @ backMeta
     

    fun isMeta PLAIN = false
      | isMeta _ = true

    fun doFrontMeta F x = map (fn M => F M x) frontMeta
    fun doBackMeta F x = map (fn M => F M x) backMeta
    fun doAllMeta F x = map (fn M => F M x) allMeta

    structure CTOrd : ORD =
    struct
      type t = contentType
      fun cmp (PLAIN,PLAIN) = EQUAL
        | cmp (PLAIN,_) = LESS
        | cmp (_,PLAIN) = GREATER
        | cmp (T1,T2) = if T1=T2 then EQUAL else
            let
              exception Term of order
              fun check M () =
                case (T1=M,T2=M) of
                    (true,_) => raise Term LESS
                  | (_,true) => raise Term GREATER
                  | _ => ()
            in
                (doAllMeta check ();raise Fail "Tried to compare unknown contentTypes")
                handle (Term O) => O
            end
    end

    datatype domAddress =
          Every of contentType 

    structure domAddOrd : ORD = 
    struct
       type t = domAddress
        fun cmp (Every MM, Every NN) = CTOrd.cmp (MM,NN)
    end
    fun domAddToString (Every MM) = "(Every " ^ (pMtoString MM) ^ ")" 


    structure DDict = MkDict(domAddOrd)

    datatype command = 
          SKIP
        | CLEAN
        | NM
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
  datatype processMode = datatype contentType

  datatype linetype =
      Blank
    | Plaintext of string
    | MetaBlock of processMode (* INVARIANT: not 'PLAIN' *)
    | MetaBlank

  val Mode = ref PLAIN

  (* If !Mode is PLAIN, then flip MM sets Mode to MM.
   *  If !Mode isnt PLAIN, then flip MM sets Mode to PLAIN *)
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
                PLAIN => MM
              | _ => PLAIN) (* TODO: Add warning if !Mode<>MM *)
    end

  structure GPF : 
  sig
    type storedLine
    type gpf
    val appendContent : gpf -> storedLine -> gpf
    val appendHead : contentType -> gpf -> storedLine -> gpf
    val Nothing : gpf
    val getContent : gpf -> storedLine list
    val getMeta : contentType -> gpf -> storedLine list
  end
  = 
  struct
    
    type storedLine = string
    
    (* INVARIANT: storedLines are stored in reverse order *)
    (* INVARIANT: raw content is listed in reverse order *)
    type gpf = linetype list * storedLine list DDict.dict 


    (* Basic utility functions *)

    (* appendContent : gpf * linetype -> gpf *)
    fun appendContent (L,DD) v' =
         ((Plaintext v')::L,
         DDict.updateCmb DD (Every PLAIN,v') op:: (fn l=>[l]))
    fun appendHead MM (L,DD) v' =
      let
        val _ = (isMeta MM) orelse raise Fail "Not Meta"
      in 
         (L,
         DDict.updateCmb DD (Every MM,v') op:: (fn l=>[l]))
      end
    fun logMeta MM (L,DD) = ((MetaBlock MM)::L,DD)

    val Nothing : gpf = ([],DDict.empty)
    fun getContent (_,DD) = List.rev (DDict.lookupDefault DD (Every PLAIN) [])
    fun getMeta MM (_,DD) = 
      let
        val _ = (isMeta MM) orelse raise Fail "Not Meta"
      in
        List.rev (DDict.lookupDefault DD (Every MM) [])
      end

  end
  open GPF

    val spaceSplit = fn s => (s,
    let val ls = String.tokens Char.isSpace s
    in
      (* print ((String.concatWith ";" ls)^"\n"); *)
       ls
    end)
    val unSpaceSplit = String.concatWith " "
    val normalize = unSpaceSplit o pr2 o spaceSplit

    fun process (_,[]) acc = (case !Mode of PLAIN => appendContent acc ""
                                         | _ => (*TODO appendContent*) acc )
      | process (_, [MMBstr as ("[$$]"|"[HH]"|"[%%]")]) acc =
            let 
               val MM = pMfromBString MMBstr
               val _ = flip MM
            in
             acc (* TODO appendContent acc (MetaBlock MM) *)
            end
      | process (_,"[$$]"::rest) acc = (flip $$; 
                                        process
                                          (unSpaceSplit rest,rest) acc
                                          (* (appendContent acc (MetaBlock $$))
                                          TODO: Change these to actually append
                                          these linetype blocks to the raw log*)
                                       )
      | process (_,"[HH]"::rest) acc = (flip HH; 
                                        process
                                          (unSpaceSplit rest,rest) acc
                                          (*appendContent acc (MetaBlock HH)*)
                                       )
      | process (_,"[%%]"::rest) acc = (flip %%; 
                                        process
                                          (unSpaceSplit rest,rest) acc
                                          (*appendContent acc (MetaBlock %%)*)
                                       )
      | process (s,tokens as t0::rest) acc = 
            (case !Mode of
                 PLAIN => appendContent acc s
              |  $$ => appendHead $$ acc (normalize s)
              |  HH => appendHead HH acc (normalize s)
              |  %% => appendHead %% acc (normalize s)
              (*|  _ => Plaintext s :: acc*)
             )
    (* fun read () = List.rev(List.foldMapl (Fn.uncurry process) spaceSplit []
      (Reader.toList path))*)

    fun read () = 
          path  |> Reader.toList                     (* string list *)
                |> map spaceSplit                    (* string * (string list) list*)
                |> foldl (Fn.uncurry process) Nothing


  datatype printFun = JustContent | All | Debug

      (* fun echo Debug (Plaintext s) = print ("[TEXTLINE:"^s^"]")
        | echo _ (Plaintext s) = print(s^"\n")

        | echo Debug Blank = print("[BLANKLINE]") 
        | echo _ Blank = print("\n")

        | echo Debug MetaBlank = print("[METABLANK]")
        | echo _ MetaBlank = print("") 

        | echo Debug (MetaBlock M) = print("[META " ^ (pMtoString M) ^ "]") 
        | echo _ (MetaBlock M) = () *)
  val indent = "    "
  fun echo doIndent s = print((if doIndent then indent else "") ^ s ^ "\n")
  fun printCont pf = ignore o (map (echo false)) o getContent
  fun printMeta pf MM G =
    let
      (*fun echo (tok,cont) = 
        print(indent ^ tok ^ " " ^ (String.concatWith " " cont) ^ "\n")*)
      fun printTag _ = print ("["^ (pMtoString  MM)^"]\n")

      val metaContents = getMeta MM G
    in
      if List.null metaContents
      then ()
      else
      (printTag(); 
       map (echo true) metaContents;
      printTag() ) 
    end
                            


  fun perform SKIP = ()
    | perform CLEAN = 
        let 
          val G = read()
        in
          ignore(
            doFrontMeta (printMeta All) G;
            printCont All G;
            doBackMeta (printMeta All) G
          )
        end
    | perform NM = 
        let 
          val G = read()
          (*val _ = print("KEYS:\n")
          val _ = map (fn k => print((domAddToString k)^"\n")) (DDict.keys G)*)
        in
          printCont All G
        end
end


