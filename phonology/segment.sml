structure Vowel = struct
    datatype height = Low
                    | LowMid
                    | Mid
                    | HighMid
                    | High

    datatype centrality = Front
                        | Central
                        | Back

    datatype vowel = Vowel of height * centrality
    
    fun vocToStr (Vowel (Low, Central)) = "a"
      | vocToStr (Vowel (Mid, Front)) = "e"
      | vocToStr (Vowel (High, Front)) = "i"
      | vocToStr (Vowel (Mid, Back)) = "o"
      | vocToStr (Vowel (High, Back)) = "u"
      | vocToStr (Vowel (HighMid, Front)) = "ɪ"
      | vocToStr (Vowel (LowMid, Front)) = "ɛ"
      | vocToStr (Vowel (HighMid, Back)) = "ʊ"
      | vocToStr (Vowel (LowMid, Back)) = "ɔ"
      | vocToStr _ = "()"
end

structure Consonant = struct
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

    datatype consonant = Consonant of voice * place * manner

    fun consToStr (Consonant (Voiceless, Bilabial, Stop)) = "p"
      | consToStr (Consonant (Voiced, Bilabial, Stop)) = "b"
      | consToStr (Consonant (Voiceless, Bilabial, NonSibil)) = "ɸ"
      | consToStr (Consonant (Voiced, Bilabial, NonSibil)) = "β"
      | consToStr (Consonant (Voiced, Bilabial, Nasal)) = "m"
      | consToStr (Consonant (Voiceless, Labiodental, NonSibil)) = "f"
      | consToStr (Consonant (Voiceless, Dental, Stop)) = "t"
      | consToStr (Consonant (Voiced, Dental, Stop)) = "d"
      | consToStr (Consonant (Voiceless, Dental, NonSibil)) = "θ"
      | consToStr (Consonant (Voiced, Dental, NonSibil)) = "ð"
      | consToStr (Consonant (Voiced, Dental, Nasal)) = "n"
      | consToStr (Consonant (Voiceless, Alveolar, Sibilant)) = "s̺"
      | consToStr (Consonant (Voiced, Alveolar, Sibilant)) = "z̺"
      | consToStr (Consonant (Voiceless, Dental, Sibilant)) = "s̪"
      | consToStr (Consonant (Voiced, Dental, Sibilant)) = "z̪"
      | consToStr (Consonant (Voiceless, Dental, Affricate)) = "ts"
      | consToStr (Consonant (Voiced, Dental, Affricate)) = "dz"
      | consToStr (Consonant (Voiceless, Palatal, Sibilant)) = "ʃ"
      | consToStr (Consonant (Voiced, Palatal, Sibilant)) = "ʒ"
      | consToStr (Consonant (Voiceless, Palatal, Affricate)) = "tʃ"
      | consToStr (Consonant (Voiced, Palatal, Affricate)) = "dʒ"
      | consToStr (Consonant (Voiced, Palatal, Nasal)) = "ɲ"
      | consToStr (Consonant (Voiced, Palatal, Lateral)) = "ʎ"
      | consToStr (Consonant (Voiced, Palatal, NonSibil)) = "ʝ"
      | consToStr (Consonant (Voiceless, Velar, Stop)) = "k"
      | consToStr (Consonant (Voiced, Velar, Stop)) = "g"
      | consToStr (Consonant (Voiceless, Labiovelar, Stop)) = "kʷ"
      | consToStr (Consonant (Voiced, Labiovelar, Stop)) = "gʷ"
      | consToStr (Consonant (Voiceless, Velar, NonSibil)) = "x"
      | consToStr (Consonant (Voiced, Velar, NonSibil)) = "ɣ"
      | consToStr (Consonant (Voiced, Palatal, Approximant)) = "j"
      | consToStr (Consonant (Voiced, Labiovelar, Approximant)) = "w"
      | consToStr (Consonant (Voiced, Dental, Lateral)) = "l"
      | consToStr (Consonant (Voiced, Dental, Trill)) = "r"
      | consToStr (Consonant (Voiced, Dental, Tap)) = "ɾ"
      | consToStr (Consonant (Voiceless, Glottal, NonSibil)) = "h"
      | consToStr _ = "()"
end

structure Seg = struct
    open Vowel Consonant

    val a = Vowel (Low, Central)
    val e = Vowel (Mid, Front)
    val i = Vowel (High, Front)
    val seg_o = Vowel (Mid, Back) (* sml uses "o" for function composition *)
    val u = Vowel (High, Back)
    val i' = Vowel (HighMid, Front)
    val e' = Vowel (LowMid, Front)
    val u' = Vowel (HighMid, Back)
    val o' = Vowel (LowMid, Back)

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
    val rr = Consonant (Voiced, Dental, Trill)
    val r = Consonant (Voiced, Dental, Tap)

    val h = Consonant (Voiceless, Glottal, NonSibil)
end
