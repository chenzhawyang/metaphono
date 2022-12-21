structure Main = struct
    fun write_file filename text =
        let val file = TextIO.openOut filename
        in 
            TextIO.output (file, text);
            TextIO.closeOut file
        end

    fun main (prog_name, args) = 
        let val filename = "./output/output.txt"
            val text = Gloss.toStr Lexicon.entries
            val _ = write_file filename text
        in 
            print ("wrote to file " ^ filename ^ ".\n");
            OS.Process.success 
        end
end
