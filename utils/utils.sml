structure Utils = struct

open Vowel Consonant Syllable PWord 

(* common predicates *)

structure Pred = struct
    fun is_stressed syll =
        let val Syllable (onset, nuc, coda, stress) = syll
        in case stress 
            of Stressed => true
             | Unstressed => false
        end
end

end
