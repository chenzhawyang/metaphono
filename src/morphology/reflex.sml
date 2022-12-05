structure Reflex = struct
    open PWord SoundChange

    datatype stage = Stage of pWord * pWord * soundChange

    fun stgToStr stage =
        let val Stage (domain, image, sc) = stage
            val origin = pWordToStr domain
            val result = pWordToStr image
            val SoundChange (_, name) = sc
        in origin ^ " => " ^ result ^ " (" ^ name ^ ")" end
end

signature REFLEX = sig

end
