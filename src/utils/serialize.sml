structure Serialize : sig
    val write_file : Reflex.t list * string -> unit
end = struct
    structure Aux : sig
        val serl_reflex : Reflex.t -> SExp.value
        val serialize : Reflex.t list -> SExp.value
    end = struct       
        fun serl_reflex (Reflex.T (pword, history)) = history
        val serialize = SExp.LIST o List.map serl_reflex
    end

    fun write_file (reflexes, filename) =
        let val file = TextIO.openOut filename
            val text = Aux.serialize reflexes
        in 
            (* TextIO.output (file, text); *)
            SExpPrinter.print (file, text);
            TextIO.closeOut file
        end
end
