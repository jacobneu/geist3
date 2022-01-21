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
      fun toList (infile) = 
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
  val print : gpf -> unit

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

  type gpf = linetype list


    val Mode = ref NORMAL

    (* If !Mode is NORMAL, then flip MM sets Mode to MM.
    *  If !Mode isnt NORMAL, then flip MM sets Mode to NORMAL *)
    fun flip MM = Mode := (case !Mode of 
                              NORMAL => (print ("NORMAL to " ^ (pMtoString MM) ^
                              "\n");
                                         MM)
                            | MM' => (  print ((pMtoString MM') ^ " to NORMAL, by " ^ 
                                     (pMtoString MM) ^ "\n");
                                      NORMAL)) (* TODO: Add warning if !Mode<>MM *)

    val spaceSplit = fn s => (s,
    let val ls = String.tokens Char.isSpace s
    in
      (* print ((String.concatWith ";" ls)^"\n"); *)
       ls
    end)
    val unSpaceSplit = String.concatWith " "

    fun process (_,[]) acc = (case !Mode of NORMAL => Blank::acc
                                         | _ => MetaBlank::acc)
      | process (_, [MMBstr as ("[$$]"|"[HH]"|"[%%]")]) acc =
            let 
               val MM = pMfromBString MMBstr
               val _ = flip MM
            in
             (MetaBlock MM) :: acc
            end
      | process (_,"[$$]"::rest) acc = (flip $$; 
                                        process
                                          (unSpaceSplit rest,rest)
                                          ((MetaBlock $$) :: acc)
                                       )
      | process (_,"[HH]"::rest) acc = (flip HH; 
                                        process
                                          (unSpaceSplit rest,rest) 
                                          ((MetaBlock HH) :: acc)
                                       )
      | process (_,"[%%]"::rest) acc = (flip %%; 
                                        process
                                          (unSpaceSplit rest,rest) 
                                          ((MetaBlock %%) :: acc)
                                       )
      | process (s,tokens as t0::rest) acc = 
            (case !Mode of
                 NORMAL => Plaintext s :: acc
              |  $$ => process (s,rest) acc (* TODO: Make it actually store the info *)
              |  HH => process (s,rest) acc (* TODO: Make it actually store the info *)
              |  %% => process (s,rest) acc (* TODO: Make it actually store the info *)
              (*|  _ => Plaintext s :: acc*)
             )

    infix |>
    fun x |> f = f x

    (* fun read () = List.rev(List.foldMapl (Fn.uncurry process) spaceSplit []
      (Reader.toList path))*)

    fun read () =
          path  |> Reader.toList
                |> map spaceSplit
                |> foldl (Fn.uncurry process) []
                |> List.rev

  val print = 
    let
      fun echo Blank =print("\n") (* print("[BLANKLINE]") *)
        | echo MetaBlank = print("") (* print("[METABLANK]") *)
        | echo (MetaBlock M) = () (* print("[META " ^ (pMtoString M) ^ "]") *)
        | echo (Plaintext s) = print(s^"\n") (* print("[TEXTLINE:"^s^"]") *)
    in
      ignore o (map echo)
    end


  fun perform SKIP = ()
    | perform CLEAN = print(read())
end


