structure OSp : EPOCH = struct
    open Changes

    val name = "Western Romance -> Old Spanish"

    val vowel_shifts = 
        let val e_diphthongization_2 = E_Diphthongization_II.rule
            val o_diphthongization = O_Diphthongization.rule
        in 
            [ e_diphthongization_2
            , o_diphthongization ]
        end

    val consonantal_shifts = 
        let val spirantization_lh = Spirantization.rule
            val debuccalization = Debuccalization.rule
            val fronting_ch = Fronting.rule
            val lenition_2 = Lenition_II.rule
        in 
            [ spirantization_lh
            , debuccalization 
            , fronting_ch 
            , lenition_2 ]
        end

    val history = vowel_shifts
                  @ consonantal_shifts

    val epoch = (name, history)
end
