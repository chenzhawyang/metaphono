signature VOWEL = sig
    exception BadVowel

    datatype height = Low
                    | LowMid
                    | Mid
                    | HighMid
                    | High

    datatype centrality = Front
                        | Central
                        | Back

    datatype t = Vowel of height * centrality

    val toStr : t -> string
end

structure Vowel : VOWEL = struct
    exception BadVowel

    datatype height = Low
                    | LowMid
                    | Mid
                    | HighMid
                    | High

    datatype centrality = Front
                        | Central
                        | Back

    datatype t = Vowel of height * centrality
    
    fun toStr (Vowel (Low, Central)) = "a"
      | toStr (Vowel (Mid, Front)) = "e"
      | toStr (Vowel (High, Front)) = "i"
      | toStr (Vowel (Mid, Back)) = "o"
      | toStr (Vowel (High, Back)) = "u"
      | toStr (Vowel (HighMid, Front)) = "ɪ"
      | toStr (Vowel (LowMid, Front)) = "ɛ"
      | toStr (Vowel (HighMid, Back)) = "ʊ"
      | toStr (Vowel (LowMid, Back)) = "ɔ"
      | toStr _ = raise BadVowel
end

signature CONSONANT = sig
    exception BadConsonant

    datatype voice = Voiced | Voiceless

    datatype place = Bilabial | Labiodental
                   | Dental
                   | Alveolar
                   | Palatal
                   | Velar | Labiovelar
                   | Glottal

    datatype manner = Nasal
                    | Stop
                    | NonSibil
                    | Sibilant
                    | Affricate
                    | Approximant
                    | Tap
                    | Trill
                    | Lateral

    datatype t = Consonant of voice * place * manner    

    val toStr : t -> string
end

structure Consonant : CONSONANT = struct
    exception BadConsonant

    datatype voice = Voiced | Voiceless

    datatype place = Bilabial | Labiodental
                   | Dental
                   | Alveolar
                   | Palatal
                   | Velar | Labiovelar
                   | Glottal

    datatype manner = Nasal
                    | Stop
                    | NonSibil
                    | Sibilant
                    | Affricate
                    | Approximant
                    | Tap
                    | Trill
                    | Lateral

    datatype t = Consonant of voice * place * manner

    fun toStr (Consonant (Voiceless, Bilabial, Stop)) = "p"
      | toStr (Consonant (Voiced, Bilabial, Stop)) = "b"
      | toStr (Consonant (Voiceless, Bilabial, NonSibil)) = "ɸ"
      | toStr (Consonant (Voiced, Bilabial, NonSibil)) = "β"
      | toStr (Consonant (Voiced, Bilabial, Nasal)) = "m"
      | toStr (Consonant (Voiceless, Labiodental, NonSibil)) = "f"
      | toStr (Consonant (Voiceless, Dental, Stop)) = "t"
      | toStr (Consonant (Voiced, Dental, Stop)) = "d"
      | toStr (Consonant (Voiceless, Dental, NonSibil)) = "θ"
      | toStr (Consonant (Voiced, Dental, NonSibil)) = "ð"
      | toStr (Consonant (Voiced, Dental, Nasal)) = "n"
      | toStr (Consonant (Voiceless, Alveolar, Sibilant)) = "s̺"
      | toStr (Consonant (Voiced, Alveolar, Sibilant)) = "z̺"
      | toStr (Consonant (Voiceless, Dental, Sibilant)) = "s̪"
      | toStr (Consonant (Voiced, Dental, Sibilant)) = "z̪"
      | toStr (Consonant (Voiceless, Dental, Affricate)) = "ts"
      | toStr (Consonant (Voiced, Dental, Affricate)) = "dz"
      | toStr (Consonant (Voiceless, Palatal, Sibilant)) = "ʃ"
      | toStr (Consonant (Voiced, Palatal, Sibilant)) = "ʒ"
      | toStr (Consonant (Voiceless, Palatal, Affricate)) = "tʃ"
      | toStr (Consonant (Voiced, Palatal, Affricate)) = "dʒ"
      | toStr (Consonant (Voiced, Palatal, Nasal)) = "ɲ"
      | toStr (Consonant (Voiced, Palatal, Lateral)) = "ʎ"
      | toStr (Consonant (Voiced, Palatal, NonSibil)) = "ʝ"
      | toStr (Consonant (Voiceless, Velar, Stop)) = "k"
      | toStr (Consonant (Voiced, Velar, Stop)) = "g"
      | toStr (Consonant (Voiceless, Labiovelar, Stop)) = "kʷ"
      | toStr (Consonant (Voiced, Labiovelar, Stop)) = "gʷ"
      | toStr (Consonant (Voiceless, Velar, NonSibil)) = "x"
      | toStr (Consonant (Voiced, Velar, NonSibil)) = "ɣ"
      | toStr (Consonant (Voiced, Palatal, Approximant)) = "j"
      | toStr (Consonant (Voiced, Labiovelar, Approximant)) = "w"
      | toStr (Consonant (Voiced, Dental, Lateral)) = "l"
      | toStr (Consonant (Voiced, Dental, Trill)) = "r"
      | toStr (Consonant (Voiced, Dental, Tap)) = "ɾ"
      | toStr (Consonant (Voiceless, Glottal, NonSibil)) = "h"
      | toStr _ = raise BadConsonant
end

structure Seg = struct
    (* vowels *)

    local open Vowel in
        val a = Vowel (Low, Central)
        val e = Vowel (Mid, Front)
        val i = Vowel (High, Front)
        val seg_o = Vowel (Mid, Back) (* sml uses "o" for function composition *)
        val u = Vowel (High, Back)
        val i' = Vowel (HighMid, Front)
        val e' = Vowel (LowMid, Front)
        val u' = Vowel (HighMid, Back)
        val o' = Vowel (LowMid, Back)
    end

    (* consonants *)

    local open Consonant in 
        val p = Consonant (Voiceless, Bilabial, Stop)
        val b = Consonant (Voiced, Bilabial, Stop)
        val ph = Consonant (Voiceless, Bilabial, NonSibil)
        val bh = Consonant (Voiced, Bilabial, NonSibil)
        val m = Consonant (Voiced, Bilabial, Nasal)
        val f = Consonant (Voiceless, Labiodental, NonSibil)
        val t = Consonant (Voiceless, Dental, Stop)
        val d = Consonant (Voiced, Dental, Stop)
        val th = Consonant (Voiceless, Dental, NonSibil)
        val dh = Consonant (Voiced, Dental, NonSibil)
        val n = Consonant (Voiced, Dental, Nasal)
        val s = Consonant (Voiceless, Alveolar, Sibilant)
        val z = Consonant (Voiced, Alveolar, Sibilant)
        val s_dent = Consonant (Voiceless, Dental, Sibilant)
        val z_dent = Consonant (Voiced, Dental, Sibilant)
        val ts = Consonant (Voiceless, Dental, Affricate)
        val dz = Consonant (Voiced, Dental, Affricate)
        val sh = Consonant (Voiceless, Palatal, Sibilant)
        val zh = Consonant (Voiced, Palatal, Sibilant)
        val ch = Consonant (Voiceless, Palatal, Affricate)
        val dg = Consonant (Voiced, Palatal, Affricate)
        val gn = Consonant (Voiced, Palatal, Nasal)
        val lh = Consonant (Voiced, Palatal, Lateral)
        val jh = Consonant (Voiced, Palatal, NonSibil)
        val k = Consonant (Voiceless, Velar, Stop)
        val g = Consonant (Voiced, Velar, Stop)
        val kw = Consonant (Voiceless, Labiovelar, Stop)
        val gw = Consonant (Voiced, Labiovelar, Stop)
        val x = Consonant (Voiceless, Velar, NonSibil)
        val gh = Consonant (Voiced, Velar, NonSibil)
        val j = Consonant (Voiced, Palatal, Approximant)
        val w = Consonant (Voiced, Labiovelar, Approximant)
        val l = Consonant (Voiced, Dental, Lateral)
        val r = Consonant (Voiced, Dental, Trill)
        val r' = Consonant (Voiced, Dental, Tap)
        val h = Consonant (Voiceless, Glottal, NonSibil)
    end
end
