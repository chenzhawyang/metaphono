signature HISTORY = sig
    val historia : (string * (Rule.t list)) list
    val pidal : Reflex.t -> Reflex.t
end

structure History : HISTORY = struct
    val historia =
        let val PrRom = PrRom.epoch
            val WRom = WRom.epoch
            val OSp = OSp.epoch
            val MSp = MSp.epoch
        in 
            [ PrRom
            , WRom
            , OSp
            , MSp ]
        end            

    val pidal = Reflex.apply_ll historia
end
