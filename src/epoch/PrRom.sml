structure PrRom = struct
    open Changes

    val name = "Latin -> Proto-Romance"

    val early_consonantal_shifts = 
        let val elide_m = ElideWordFinalConsonant.mk Seg.m
            val deaspiration = Deaspiration.rule
            val prothesis = Prothesis.rule
        in 
            [ elide_m
            , deaspiration 
            , prothesis ]
        end

    val vowel_shifts = 
        let val monophthongization =
                Monophthongization.rule
            val loss_of_quant = LossOfQuant.rule
            val great_merger = GreatMerger.rule
            val atonic_merger = AtonicMerger.rule
            val metaphony = Metaphony.rule
        in 
            [ monophthongization
            , loss_of_quant
            , great_merger 
            , atonic_merger 
            , metaphony ] 
        end

    val consonantal_shifts =
        let val fortition_j = Fortition.mk (Seg.j, Seg.dg)
            val fortition_w = Fortition.mk (Seg.w, Seg.bh)
        in 
            [ fortition_j 
            , fortition_w ]
        end

    val palatalizations = 
        let val dental_palatalization = [T_Palatalization.rule, D_Palatalization.rule]
            val velar_palatalization = [K_Palatalization.rule, G_Palatalization.rule]
            val lateral_palatalization = LateralPalatalization.rule
            val nasal_palatalization = NasalPalatalization.rule
        in 
            dental_palatalization
            @ velar_palatalization
            @ [lateral_palatalization] 
            @ [nasal_palatalization]
        end

    val history = early_consonantal_shifts
                  @ vowel_shifts
                  @ consonantal_shifts
                  @ palatalizations

    val epoch = (name, history)
end
