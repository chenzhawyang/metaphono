structure MSp = struct
    open Changes

    val name = "Old Spanish -> Modern Spanish"

    val consonantal_shifts =
        let val betacism = Betacism.rule
            val deaspiration = Deaspiration.rule
            val labiodentalization = Labiodentalization.rule
            val deaffrication_palatal = Deaffrication_Palatal.rule
            val deaffrication_dental = Deaffrication_Dental.rule
            val devoicing_sibilant = DevoicingSibilant.rule
            val desibilation = Desibilation.rule
            val retraction_sh = Retraction.rule
        in 
            [ betacism
            , deaspiration
            , labiodentalization
            , deaffrication_palatal
            , deaffrication_dental
            , devoicing_sibilant
            , desibilation
            , retraction_sh
            ]
        end

    val history = consonantal_shifts

    val epoch = (name, history)
end
