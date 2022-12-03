structure PrRom = struct

open Vowel Consonant Syllable Rule Utils

val epoch = "Latin -> Proto-Romance"

structure LossOfQuantity : RULE = struct
    val rewrite = let fun f x = x in Onsetism f end
    val context = NoContext
    val name = "haha"
end

local structure sc = SoundChangeFun (LossOfQuantity)
in val loss_of_quantity = sc.sound_change end

local val sc1 = loss_of_quantity
in val history = [sc1] end

end
