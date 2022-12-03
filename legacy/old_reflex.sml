structure Reflex = struct
    open PWord SoundChange

    (* reflext with its phonological history *)

    datatype reflex = Reflex of pWord * string

    fun mkEtymon pword = let val name = (pWordToStr pword) ^ " <Etymon>\n"
                       in Reflex (pword, name) end                                 

    (* print reflex *)

    fun printRefl (Reflex (_, history)) = print history            

    (* apply the sound change to a word and keep a record of its history *)

    fun newHistory pword1 pword2 name
        =  (pWordToStr pword1) ^ " => " ^ (pWordToStr pword2) ^ " (" ^ name ^ ")\n"

    (* apply a sound change *)

    fun applySC (SoundChange (f, name)) (Reflex (pword, history))
        = let val pword' = f pword
              val history' = if pword = pword'
                             then history
                             else history ^ (newHistory pword pword' name)
          in Reflex (pword', history') end

    (* apply a list of sound changes *)

    fun applyChainShift [] reflex = reflex
      | applyChainShift (x :: xs) reflex
        = let val reflex' = applySC x reflex
          in applyChainShift xs reflex' end

    fun applyLangShift l reflex = applyChainShift (List.concat l) reflex

    fun MenendezPidal l reflex = applyLangShift (List.concat l) reflex
end
