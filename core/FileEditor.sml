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

  datatype linetype =
      Blank
    | Plaintext of string


  type gpf = linetype list

  fun process "" = Blank
    | process s = Plaintext s

  fun read () = map process (Reader.toList path)
  val print = 
    let
      fun echo Blank = print("-------------------\n")
        | echo (Plaintext s) = print(s^"\n")
    in
      ignore o (map echo)
    end

  fun perform SKIP = ()
    | perform CLEAN = print(read())
end


