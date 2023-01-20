structure Serialize : sig
    val write_file : Reflex.t list * string -> unit
end = struct
    structure Aux : sig
        val serl_reflex : Reflex.t -> string
        val serialize : Reflex.t list -> string list
    end = struct       
        fun quote s = "\"" ^ s ^ "\""
        fun parenthesize s = "(" ^ s ^ ")"
        fun serl_reflex (Reflex.T (_, [])) = ""
          | serl_reflex (Reflex.T (pword, history)) = 
            let val aux = fn (pword1, pword2, name) => 
                             let val s = String.concatWith (" ") (List.map quote [pword1, pword2, name])
                             in parenthesize s end
            in parenthesize (String.concatWith "\n" (List.map aux history)) end

        val serialize = List.map serl_reflex
    end

    fun write_file (reflexes, filename) =
        let val file = TextIO.openOut filename
            val text_l = Aux.serialize reflexes
            fun aux [] = ()
              | aux (x :: xs) = 
                let val _ = TextIO.output (file, x ^ "\n\n")
                in aux xs end
        in 
            aux text_l;
            TextIO.closeOut file
        end
end
