structure Rule = struct
    open Syllable

    (* rewrites that changes a constituent of the syllable *)

    datatype rewrite = Onsetism of (onset -> onset)
                     | Nuxism of (nucleus -> nucleus)
                     | Codism of (coda -> coda)
                     | Syllabism of (syllable -> syllable)

    datatype context = NoContext
                     | Predicate of (syllable -> bool)
                     | WordInit
                     | WordFinal
end

local open Rule in
    signature RULE = sig
        val rewrite : rewrite
        val context : context
        val name : string
    end
end

structure SoundChange = struct
    open PWord Rule

    (* sound changes with a name *)
    (* this string is the name of the sound change *)
    datatype soundChange = SoundChange of (pWord -> pWord) * string
end

local open PWord SoundChange in
    signature SOUND_CHANGE = sig
        val name : string
        val change : pWord -> pWord
        val sound_change : soundChange
    end
end

structure SCUtils = struct
    open Syllable Rule SoundChange

    fun mkSyllabism rewrite syll =
        let val Syllable (onset, nuc, coda, stress) = syll
        in case rewrite 
            of Onsetism f => let val onset' = f onset 
                            in Syllable (onset', nuc, coda, stress) end
            | Nuxism f => let val nuc' = f nuc
                          in Syllable (onset, nuc', coda, stress) end
            | Codism f => let val coda' = f coda
                          in Syllable (onset, nuc, coda', stress) end
            | Syllabism f => f syll
        end

    fun applyOnly pred f [] = []
      | applyOnly pred f (x :: xs)
        = case pred x of
              true => f x :: applyOnly pred f xs
            | false => x :: applyOnly pred f xs

    fun applyFirst f [] = []
      | applyFirst f (x :: xs) = f x :: xs

    fun applyLast f l = let val l' = applyFirst f (rev l)
                        in rev l' end

    fun mkSoundChange syllabism context
        = case context of
              NoContext => map syllabism
            | Predicate p => applyOnly p syllabism
            | WordInit  => applyFirst syllabism
            | WordFinal => applyLast syllabism
end

functor SoundChangeFun (Rule : RULE) : SOUND_CHANGE = struct
    open SoundChange SCUtils

    val name = Rule.name

    val change = let val syllabism = mkSyllabism Rule.rewrite
                     val context = Rule.context
                 in mkSoundChange syllabism context end

    val sound_change = SoundChange (change, name)
end
