structure Rule = struct
    open Syllable

    (* rewrites that changes a constituent of the syllable *)

    datatype rewrite = Onsetism of (onset -> onset)
                     | Nucleusism of (nucleus -> nucleus)
                     | Codism of (coda -> coda)

    (* syllabism that rewrites a syllable to another *)

    datatype context = NoContext
                     | Predicate of (syllable -> bool)
                     | WordInit
                     | WordFinal

    datatype syllabism = Syllabism of (syllable -> syllable) * context

    fun evalRewrite (Onsetism f) (Syllable (on, nuc, cod, stress))
        = let val on' = f on
          in Syllable (on', nuc, cod, stress) end
      | evalRewrite (Nucleusism f) (Syllable (on, nuc, cod, stress))
        = let val nuc' = f nuc
          in Syllable (on, nuc', cod, stress) end
      | evalRewrite (Codism f) (Syllable (on, nuc, cod, stress))
        = let val cod' = f cod
          in Syllable (on, nuc, cod', stress) end

    fun mkSyllabism rewrite context
        = let val f = evalRewrite rewrite
          in Syllabism (f, context) end    
end

structure SoundChange = struct
    open PWord Rule

    (* sound changes with a name *)
    (* this string is the name of the sound change *)
    datatype soundChange = SoundChange of (pWord -> pWord) * string

    fun applyOnly pred f [] = []
      | applyOnly pred f (x :: xs)
        = case pred x of
              true => f x :: applyOnly pred f xs
            | false => x :: applyOnly pred f xs

    fun applyFirst f [] = []
      | applyFirst f (x :: xs) = f x :: xs

    fun applyLast f l = let val l' = applyFirst f (rev l)
                        in rev l' end

    fun mkSoundChange (Syllabism (f, context)) name
        = case context of
              NoContext => let val sc = map f
                           in SoundChange (sc, name) end
            | Predicate p => let val sc = applyOnly p f
                             in SoundChange (sc, name) end
            | WordInit  => let val sc = applyFirst f
                           in SoundChange (sc,  name) end
            | WordFinal => let val sc = applyLast f
                           in SoundChange (sc, name) end
end
