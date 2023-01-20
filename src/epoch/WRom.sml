structure WRom = struct
    open Changes

    val name = "Proto-Romance -> Western Romance"

    val vowel_shifts = 
        let val e_diphthongization = E_Diphthongization.rule
        in 
            [ e_diphthongization ] 
        end

    val consonantal_shifts =
        let val lenition_1 = Lenition_I.rule
            val degemination_1 = Degemination_I.rule
        in 
            [ lenition_1
            , degemination_1 ] 
        end

    val history = vowel_shifts
                  @ consonantal_shifts

    val epoch = (name, history)
end
