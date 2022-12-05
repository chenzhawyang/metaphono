structure Lemma = struct
    open Noun PWord

    datatype category = Nominal of gender * caso * number
                      | Numeral

    fun catToStr (Nominal (gender, caso, number)) = nomToStr gender caso number
      | catToStr Numeral = "num."

    datatype Lemma = Lemma of pWord * category
end
