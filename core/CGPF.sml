structure Reader = 
struct
    local
      val clean = 
        String.implode o (List.filter (Fn.curry op<> #"\n")) o String.explode
    in
      fun iterate (infile:string) step term last = 
        let
          val ins = TextIO.openIn infile
          fun loop ins =
            case TextIO.inputLine ins of
              SOME line => step line (loop ins)
            | NONE      => term()
         in
           last (loop ins before TextIO.closeIn ins)
         end
       fun toList = iterate (Fn.curry op::) (fn () => []) (map clean)
    end
end




structure Content
