fun build () =
    let val cmd = "ml-build ./src/metaphono.cm Main.main ./bin/main"
    in OS.Process.system cmd end
    
fun run () = 
    let val cmd = "sml @SMLload ./bin/main.amd64-linux"
    in OS.Process.system cmd end

val load = CM.autoload "./src/metaphono.cm"
