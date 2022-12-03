datatype height = Low
                | LowMid
                | Mid
                | HighMid
                | High

datatype centrality = Front
                    | Central
                    | Back

datatype vowel = Vowel of height * centrality

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

val seg_a = Vowel (Low, Central)
val seg_e = Vowel (Mid, Front)
val seg_i = Vowel (High, Front)
val seg_o = Vowel (Mid, Back)
val seg_u = Vowel (High, Back)
val seg_i' = Vowel (HighMid, Front)
val seg_e' = Vowel (LowMid, Front)
val seg_u' = Vowel (HighMid, Back)
val seg_o' = Vowel (LowMid, Back)

fun vocToStr (Vowel (Low, Central)) = "a"
  | vocToStr (Vowel (Mid, Front)) = "e"
  | vocToStr (Vowel (High, Front)) = "i"
  | vocToStr (Vowel (Mid, Back)) = "o"
  | vocToStr (Vowel (High, Back)) = "u"
  | vocToStr (Vowel (HighMid, Front)) = "\201\170"
  | vocToStr (Vowel (LowMid, Front)) = "\201\155"
  | vocToStr (Vowel (HighMid, Back)) = "\202\138"
  | vocToStr (Vowel (LowMid, Back)) = "\201\148"
  | vocToStr _ = "()"

val seg_p = Consonant (Voiceless, Bilabial, Stop)
val seg_b = Consonant (Voiced, Bilabial, Stop)
val seg_ph = Consonant (Voiceless, Bilabial, NonSibil)
val seg_bh = Consonant (Voiced, Bilabial, NonSibil)
val seg_m = Consonant (Voiced, Bilabial, Nasal)

val seg_f = Consonant (Voiceless, Labiodental, NonSibil)

val seg_t = Consonant (Voiceless, Dental, Stop)
val seg_d = Consonant (Voiced, Dental, Stop)
val seg_th = Consonant (Voiceless, Dental, NonSibil)
val seg_dh = Consonant (Voiced, Dental, NonSibil)
val seg_n = Consonant (Voiced, Dental, Nasal)

val seg_s = Consonant (Voiceless, Alveolar, Sibilant)
val seg_z = Consonant (Voiced, Alveolar, Sibilant)

val seg_s_dent = Consonant (Voiceless, Dental, Sibilant)
val seg_z_dent = Consonant (Voiced, Dental, Sibilant)

val seg_ts = Consonant (Voiceless, Dental, Affricate)
val seg_dz = Consonant (Voiced, Dental, Affricate)

val seg_sh = Consonant (Voiceless, Palatal, Sibilant)
val seg_zh = Consonant (Voiced, Palatal, Sibilant)

val seg_ch = Consonant (Voiceless, Palatal, Affricate)
val seg_dg = Consonant (Voiced, Palatal, Affricate)

val seg_gn = Consonant (Voiced, Palatal, Nasal)
val seg_lh = Consonant (Voiced, Palatal, Lateral)
val seg_jh = Consonant (Voiced, Palatal, NonSibil)

val seg_k = Consonant (Voiceless, Velar, Stop)
val seg_g = Consonant (Voiced, Velar, Stop)
val seg_kw = Consonant (Voiceless, Labiovelar, Stop)
val seg_gw = Consonant (Voiced, Labiovelar, Stop)
val seg_x = Consonant (Voiceless, Velar, NonSibil)
val seg_gh = Consonant (Voiced, Velar, NonSibil)

val seg_j = Consonant (Voiced, Palatal, Approximant)
val seg_w = Consonant (Voiced, Labiovelar, Approximant)

val seg_l = Consonant (Voiced, Dental, Lateral)
val seg_rr = Consonant (Voiced, Dental, Trill)
val seg_r = Consonant (Voiced, Dental, Tap)

val seg_h = Consonant (Voiceless, Glottal, NonSibil)

fun consToStr (Consonant (Voiceless, Bilabial, Stop)) = "p"
  | consToStr (Consonant (Voiced, Bilabial, Stop)) = "b"
  | consToStr (Consonant (Voiceless, Bilabial, NonSibil)) = "\201\184"
  | consToStr (Consonant (Voiced, Bilabial, NonSibil)) = "\206\178"
  | consToStr (Consonant (Voiced, Bilabial, Nasal)) = "m"
  | consToStr (Consonant (Voiceless, Labiodental, NonSibil)) = "f"
  | consToStr (Consonant (Voiceless, Dental, Stop)) = "t"
  | consToStr (Consonant (Voiced, Dental, Stop)) = "d"
  | consToStr (Consonant (Voiceless, Dental, NonSibil)) = "\206\184"
  | consToStr (Consonant (Voiced, Dental, NonSibil)) = "\195\176"
  | consToStr (Consonant (Voiced, Dental, Nasal)) = "n"
  | consToStr (Consonant (Voiceless, Alveolar, Sibilant)) = "s" ^ "\204\186"
  | consToStr (Consonant (Voiced, Alveolar, Sibilant)) = "z" ^ "\204\186"
  | consToStr (Consonant (Voiceless, Dental, Sibilant)) = "s" ^ "\204\170"
  | consToStr (Consonant (Voiced, Dental, Sibilant)) = "z" ^ "\204\170"
  | consToStr (Consonant (Voiceless, Dental, Affricate)) = "\202\166"
  | consToStr (Consonant (Voiced, Dental, Affricate)) = "\202\163"
  | consToStr (Consonant (Voiceless, Palatal, Sibilant)) = "\202\131"
  | consToStr (Consonant (Voiced, Palatal, Sibilant)) = "\202\146"
  | consToStr (Consonant (Voiceless, Palatal, Affricate)) = "\202\167"
  | consToStr (Consonant (Voiced, Palatal, Affricate)) = "\202\164"
  | consToStr (Consonant (Voiced, Palatal, Nasal)) = "\201\178"
  | consToStr (Consonant (Voiced, Palatal, Lateral)) = "\202\142"
  | consToStr (Consonant (Voiced, Palatal, NonSibil)) = "\202\157"
  | consToStr (Consonant (Voiceless, Velar, Stop)) = "k"
  | consToStr (Consonant (Voiced, Velar, Stop)) = "g"
  | consToStr (Consonant (Voiceless, Labiovelar, Stop)) = "k" ^ "\202\183"
  | consToStr (Consonant (Voiced, Labiovelar, Stop)) = "g" ^ "\202\183"
  | consToStr (Consonant (Voiceless, Velar, NonSibil)) = "x"
  | consToStr (Consonant (Voiced, Velar, NonSibil)) = "\201\163"
  | consToStr (Consonant (Voiced, Palatal, Approximant)) = "j"
  | consToStr (Consonant (Voiced, Labiovelar, Approximant)) = "w"
  | consToStr (Consonant (Voiced, Dental, Lateral)) = "l"
  | consToStr (Consonant (Voiced, Dental, Trill)) = "r"
  | consToStr (Consonant (Voiced, Dental, Tap)) = "\201\190"
  | consToStr (Consonant (Voiceless, Glottal, NonSibil)) = "h"
  | consToStr _ = "()"

datatype onset = ZeroOnset
               | Onset of consonant
               | POnset of consonant * consonant
               | OnsetM of consonant * consonant
               | POnsetM of consonant * consonant * consonant

datatype nucleus = Monophthong of vowel
                 | Diphthong of vowel * vowel
                 | LongVowel of vowel

datatype coda = ZeroCoda
              | Codetta of consonant
              | CodaC of consonant * consonant
              | CodaCC of consonant * consonant * consonant

datatype stress = Stressed | Unstressed

datatype syllable = Syllable of onset * nucleus * coda * stress

(* syllable weight *)

datatype weight = Light | Heavy

(* fun weight (Syllable (_, rhyme, _)) = *)
(*     let fun weight' (Rhyme (Monophthong _, ZeroCoda)) = Light *)
(*        | weight' _ = Heavy *)
(*     in weight' rhyme end *)

(* assign stress *)

(* print syllables *)

fun onsToStr ZeroOnset = ""
  | onsToStr (Onset cons) = consToStr cons
  | onsToStr (POnset (cons1, cons2)) = (consToStr cons1) ^ (consToStr cons2)
  | onsToStr (OnsetM (cons1, cons2)) = (consToStr cons1) ^ (consToStr cons2)
  | onsToStr (POnsetM (cons1, cons2, cons3)) = (consToStr cons1) ^ (consToStr cons2) ^ (consToStr cons3)

fun nucToStr (Monophthong v) = vocToStr v
  | nucToStr (Diphthong (v1, v2)) = (vocToStr v1) ^ (vocToStr v2)
  | nucToStr (LongVowel v) = vocToStr v ^ "\203\144"

fun codToStr ZeroCoda = ""
  | codToStr (Codetta cons) = consToStr cons
  | codToStr (CodaC (cons1, cons2)) = (consToStr cons1) ^ (consToStr cons2)
  | codToStr (CodaCC (cons1, cons2, cons3)) = (consToStr cons1) ^ (consToStr cons2) ^ (consToStr cons3)

fun syllToStr (Syllable (on, nuc, cod, stress)) =
    let val on' = onsToStr on
        val nuc' = nucToStr nuc
        val cod' = codToStr cod
    in if stress = Stressed
       then "'" ^ on' ^ nuc' ^ cod'
       else on' ^ nuc' ^ cod'
    end

(* phonological word *)

type pWord = syllable list

(* print pWord *)

fun pWordToStr [] = ""
  | pWordToStr (x :: []) = syllToStr x
  | pWordToStr (x :: xs) = (syllToStr x) ^ "." ^ (pWordToStr xs)

fun pWordToStrLn pword = (pWordToStr pword) ^ "\n"

fun printPWord pword = (print o pWordToStr) pword

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

fun mkSyllF (Onsetism f) (Syllable (on, nuc, cod, stress))
    = let val on' = f on
      in Syllable (on', nuc, cod, stress) end
  | mkSyllF (Nucleusism f) (Syllable (on, nuc, cod, stress))
    = let val nuc' = f nuc
      in Syllable (on, nuc', cod, stress) end
  | mkSyllF (Codism f) (Syllable (on, nuc, cod, stress))
    = let val cod' = f cod
      in Syllable (on, nuc, cod', stress) end

fun mkSyllabism rewrite context
    = let val f = mkSyllF rewrite
      in Syllabism (f, context) end

(* sound changes with a name *)

datatype soundChange = SoundChange of (pWord -> pWord) * string (* this string is the name of the sound change *)

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

(* reflext with its phonological history *)

datatype reflex = Reflex of pWord * string

fun mkEtymon pword = let val name = (pWordToStr pword) ^ " <Etymon>\n"
                   in Reflex (pword, name) end

(* apply the sound change to a word and keep a record of its history *)

fun newHistory pword1 pword2 name
    =  (pWordToStr pword1) ^ " => " ^ (pWordToStr pword2) ^ " (" ^ name ^ ")\n"

fun applySC (SoundChange (f, name)) (Reflex (pword, history))
    = let val pword' = f pword
          val history' = if pword = pword'
                         then history
                         else history ^ (newHistory pword pword' name)
      in Reflex (pword', history') end

(* print reflex *)

fun printRefl (Reflex (_, history)) = print history

(* list of sound changes *)

fun applyChainShift [] reflex = reflex
  | applyChainShift (x :: xs) reflex
    = let val reflex' = applySC x reflex
      in applyChainShift xs reflex' end

fun applyLangShift l reflex = applyChainShift (List.concat l) reflex

fun MenendezPidal l reflex = applyLangShift (List.concat l) reflex
