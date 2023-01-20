structure Main : sig 
    val main : string * string list -> OS.Process.status
end = struct
    (* fun write_file filename text = *)
    (*     let val file = TextIO.openOut filename *)
    (*     in  *)
    (*         TextIO.output (file, text); *)
    (*         TextIO.closeOut file *)
    (*     end *)
            
    fun main (prog_name, args) = 
        let val filename = "./output/output.lisp"
            val reflexes = List.map History.pidal 
                                (Parse.parse_file "./data/lexicon.lisp")
            val _ = Serialize.write_file (reflexes, filename)
        in 
            print ("wrote to file " ^ filename ^ ".\n");
            OS.Process.success 
        end
end

val _ = let val prog_name = CommandLine.name ()
            val args = CommandLine.arguments ()
        in Main.main (prog_name, args) end
