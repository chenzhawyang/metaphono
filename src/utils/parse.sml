structure Parse : sig
    val parse_file : string -> Reflex.t list                  
end = struct
    structure Aux : sig
        val toStr : SExp.value -> string
        val uncons : SExp.value -> SExp.value list
        val syllabify : SExp.value -> Syll.t
        val lexemize : SExp.value -> Reflex.t
        val lexiconize : SExp.value list -> Reflex.t list
    end = struct
    (* remember that this module does not handle exceptions _at all_ *)
        fun toStr (SExp.STRING s) = s (* crushes when it needs to *)
        fun uncons (SExp.LIST l) = l  (* crushes when it needs to *)
        val syllabify = Syll.fromStrLL
                        o List.map (List.map toStr o uncons)
                        o uncons
        val lexemize = Reflex.fromSylls o List.map syllabify o uncons
        val lexiconize = List.map lexemize
    end

    val parse_file = Aux.lexiconize o SExpParser.parseFile
end
