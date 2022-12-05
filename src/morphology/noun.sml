structure Noun = struct
    datatype gender = Masc
                    | Fem
                    | Nut

    fun genderToStr Masc = "masc."
      | genderToStr Fem = "fem."
      | genderToStr Nut = "nut."

    datatype caso = Nom
                  | Voc
                  | Acc
                  | Gen
                  | Loc
                  | Dat
                  | Abl

    fun casoToStr Nom = "nom."
      | casoToStr Voc = "voc."
      | casoToStr Acc = "acc."
      | casoToStr Gen = "gen."
      | casoToStr Loc = "loc."
      | casoToStr Dat = "dat."
      | casoToStr Abl = "abl."

    datatype number = Sg
                    | Pl

    fun numToStr Sg = "sg."
      | numToStr Pl = "pl."

    fun nomToStr gender caso number =
        let val gender' = genderToStr gender
            val caso' = casoToStr caso
            val number' = numToStr number
        in gender' ^ caso' ^ number' end
end
