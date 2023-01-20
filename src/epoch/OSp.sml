structure OSp = struct
    open Changes

    (* val name = "Western Romance -> Old Spanish" *)

    val vowel_shifts = 
        let val e_diphthongization_2 = E_Diphthongization_II.rule
            val o_diphthongization = O_Diphthongization.rule
            val final_vowel_reduction = FinalVowelReduction.rule
            val apocope = Apocope.rule
        in 
            [ e_diphthongization_2
            , o_diphthongization 
            , final_vowel_reduction ]
        end

    val consonantal_shifts = 
        let val spirantization_lh = Spirantization.rule
            val debuccalization = Debuccalization.rule
            val fronting_ch = Fronting.rule
            val lenition_2 = Lenition_II.rule
            val degemination_l_n = Degemination_L_N.rule
        in 
            [ spirantization_lh
            , debuccalization 
            , fronting_ch 
            , lenition_2 
            , degemination_l_n ]
        end

    val apocope = Apocope.rule

    val history = vowel_shifts
                  @ consonantal_shifts
                  @ [apocope]

    val epoch = history
end
