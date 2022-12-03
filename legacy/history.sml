use "phonology.sml";

(* proto-romance vocalism *)

(* loss of vowel quantity *)

fun loss_quant_subst (Monophthong v)
    = let fun f (Vowel (High, height)) = Vowel (HighMid, height)
	        | f (Vowel (Mid, height)) = Vowel (LowMid, height)
	        | f v = v
      in Monophthong (f v) end          
  | loss_quant_subst (LongVowel v) = Monophthong v
  | loss_quant_subst nuc = nuc

val loss_quant_rewrite = Nucleusism loss_quant_subst

val loss_quant
    = let val syllabism = mkSyllabism loss_quant_rewrite NoContext
          val name = "Loss of Vowel Quantity"
      in mkSoundChange syllabism name end

(* dehiaticization *)

structure Dehiaticization = struct

end

(* great merger *)

fun great_merger_subst (Monophthong v)
    = let fun f (Vowel (HighMid, Front)) = Vowel (Mid, Front)
            | f (Vowel (HighMid, Back)) = Vowel (Mid, Back)
            | f v = v
      in Monophthong (f v) end
  | great_merger_subst nuc = nuc

val great_merger_rewrite = Nucleusism great_merger_subst

val great_merger
    = let val syllabism = mkSyllabism great_merger_rewrite NoContext
          val name = "Great Merger"
      in mkSoundChange syllabism name end

(* atonic merger *)

fun atonic_merger_subst (Monophthong v)
    = let fun f (Vowel (LowMid, Front)) = Vowel (Mid, Front)
            | f (Vowel (LowMid, Back)) = Vowel (Mid, Back)
            | f _ = v
      in Monophthong (f v) end
  | atonic_merger_subst nuc = nuc

val atonic_merger_rewrite = Nucleusism atonic_merger_subst

fun not_stressed (Syllable (on, nuc, cod, Unstressed)) = true
  | not_stressed _ = false
                         
val atonic_merger
    = let val syllabism = mkSyllabism atonic_merger_rewrite (Predicate not_stressed)
          val name = "Atonic Merger"
      in mkSoundChange syllabism name end

(* monophthongization *)

fun mono_subst' (diph as Diphthong (v1, v2))
    = let fun f (Vowel (Mid, Back)) (Vowel (Mid, Front)) = LongVowel (Vowel (Mid, Front))
            | f (Vowel (Low, Central)) (Vowel (High, Back)) = LongVowel (Vowel (Mid, Back))
            | f (Vowel (Low, Central)) (Vowel (Mid, Front)) = Monophthong (Vowel (Mid, Front))
            | f _ _ = diph
      in f v1 v2 end
  | mono_subst' nuc = nuc

fun mono_subst nuc
    = case nuc of
          Diphthong _ => mono_subst' nuc
       |  _ => nuc

val mono_rewrite = Nucleusism mono_subst

val mono = let val syllabism = mkSyllabism mono_rewrite NoContext
               val name = "Monophthongization"
           in mkSoundChange syllabism name end

(* proto-romance vowel shift *)

local val sc1 = mono
      val sc2 = loss_quant
      val sc3 = great_merger
      val sc4 = atonic_merger
in val prRom_vowel_shift = [sc1, sc2, sc3, sc4] end

(* proto-romance consonantism *)

(* elision of word-final [m] *)

fun elide_m_subst cod
    = case cod of
          Codetta c => if c = seg_m
                      then ZeroCoda
                      else cod
        | _ => cod
                  
val elide_m_rewrite = Codism elide_m_subst

val elide_m = let val syllabism = mkSyllabism elide_m_rewrite WordFinal
                  val name = "Elision of word-final [m]"
              in mkSoundChange syllabism name end

(* deaspiration of [h] *)

fun deasp_subst onset
    = case onset of
          Onset on => if on = seg_h
                     then ZeroOnset
                     else onset
        | _ => onset

val deasp_rewrite = Onsetism deasp_subst

val deasp = let val syllabism = mkSyllabism deasp_rewrite NoContext
                val name = "Deaspiration of [h]"
            in mkSoundChange syllabism name end

(* fortition of [w]*)

fun fortition_w_subst onset
    = case onset of
          Onset on => if on = seg_w
                      then Onset seg_bh
                      else onset
        | _ => onset

val fortition_w_rewrite = Onsetism fortition_w_subst

val fortition_w = let val syllabism = mkSyllabism fortition_w_rewrite NoContext
                      val name = "Fortition of [w]"
                  in mkSoundChange syllabism name end

(* fortition of [j] *)

structure FortitionJ = struct
fun subst onset
    = case onset of
          Onset on => if on = seg_j
                      then Onset seg_dg
                      else onset
        | _ => onset

val sound_change =
    let val rewrite = Onsetism subst
        val syllabism = mkSyllabism rewrite NoContext
        val name = "Fortition of [j]"
    in mkSoundChange syllabism name end
end

val fortition_j = FortitionJ.sound_change

(* prothesis *)

structure Prothesis = struct
val proclitic = let val on = ZeroOnset
                    val nuc = Monophthong seg_i
                    val cod = Codetta seg_s
                in Syllable (on, nuc, cod, Unstressed) end

fun prothesize syll =
    let val Syllable (on, nuc, cod, stress) = syll
        val on' = case on of
                      POnset (cons1, cons2) => Onset cons2
                    | POnsetM (cons1, cons2, cons3) => OnsetM (cons2, cons3)
                    | _ => on
        val syll' = Syllable (on', nuc, cod, stress)
    in (proclitic, syll') end

fun pred [] = false
  | pred (x :: xs)
    = let val Syllable (on, nuc, cod, stress) = x
      in
          case on of
              POnset (cons1, cons2) => (cons1 = seg_s)
            | POnsetM (cons1, cons2, cons3) => (cons1 = seg_s)
            | _ => false
      end

fun pWordF pword =
    let val (x :: xs) = pword
    in
        if pred pword
        then let val (syll1, syll2) = prothesize x
             in (syll1 :: syll2 :: xs) end
        else pword
    end

val sound_change = let val name = "Prothesis"
                   in SoundChange (pWordF, name) end
end

val prothesis = Prothesis.sound_change

(* palatalization *)

structure Palatalize = struct
fun mkSubst input output onset =
    case onset of
        Onset cons => if cons = input
                      then Onset output
                      else onset
      | _ => onset

fun mkPal input output nucleusism pred name =
    let val subst = mkSubst input output
        fun f syll =
            let val Syllable (on, nuc, cod, stress) = syll
                val on' = subst on
                val nuc' = nucleusism nuc
            in Syllable (on', nuc', cod, stress) end
        val syllabism = Syllabism (f, pred)
    in mkSoundChange syllabism name end
end

fun id x = x

structure PrRomDentalPalatalization = struct
fun mkDentPal input output =
    let fun nucleusism nuc =
            case nuc of
                Diphthong (v1, v2) => Monophthong v2
              | _ => nuc
        fun p syll =
            let val Syllable (on, nuc, cod, stress) = syll
                val onset_context =
                    case on of
                        Onset cons => cons = input
                      | _ => false
                val nuc_context =
                    case nuc of
                        Diphthong (v1, v2) => v1 = seg_i
                      | _ => false
            in onset_context andalso nuc_context end
        val pred = Predicate p
        val name = "Palatalization of [" ^ consToStr input ^ "]"
    in Palatalize.mkPal input output nucleusism pred name end
end

structure PrRomVelarPalatalization = struct
fun mkVelarPal input output =
    let fun nucleusism nuc =
            case nuc of
                Monophthong _ => nuc
              | Diphthong (v1, v2) => Monophthong v2
              | _ => nuc
        fun p syll =
            let val Syllable (on, nuc, cod, stress) = syll
                val onset_context =
                    case on of
                        Onset cons => cons = input
                      | _ => false
                val nuc_context =
                    case nuc of
                        Monophthong v =>
                        v = seg_i orelse v = seg_e orelse v = seg_e'
                     |  Diphthong (v1, v2) => v1 = seg_i
                     | _ => false
            in onset_context andalso nuc_context end
        val pred = Predicate p
        val name = "Palatalization of [" ^ consToStr input ^ "]"
    in Palatalize.mkPal input output nucleusism pred name end
end

(* palatalization of [t] *)

val palatalize_t = PrRomDentalPalatalization.mkDentPal seg_t seg_ts

(* palatalization of [d] *)

val palatalize_d = PrRomDentalPalatalization.mkDentPal seg_d seg_dg

(* palatalization of [k] *)

val palatalize_k = PrRomVelarPalatalization.mkVelarPal seg_k seg_ch

(* palatalization of [g] *)

val palatalize_g = PrRomVelarPalatalization.mkVelarPal seg_g seg_dg

(* palatalization of [l] *)

structure PrRomLateralPalatalization = struct
fun mkLatPal input output =
    let fun nucleusism nuc =
            case nuc of
                Diphthong (v1, v2) => Monophthong v2
              | _ => nuc
        fun p syll =
            let val Syllable (on, nuc, cod, stress) = syll
                val onset_context =
                    case on of
                        Onset cons => cons = input
                      | _ => false
                val nuc_context =
                    case nuc of
                        Diphthong (v1, v2) => v1 = seg_i
                      | _ => false
            in onset_context andalso nuc_context end
        val pred = Predicate p
        val name = "Palatalization of [" ^ consToStr input ^ "]"
    in Palatalize.mkPal input output nucleusism pred name end
end

val palatalize_l = PrRomLateralPalatalization.mkLatPal seg_l seg_lh

(* proto-romance consonantal shift *)

local val sc1 = elide_m
      val sc2 = deasp
      val sc3 = prothesis
in val prRom_early_cons_shift = [sc1, sc2, sc3] end

local val sc1 = fortition_w
      val sc2 = fortition_j
in val prRom_cons_shift = [sc1, sc2] end

local val sc1 = palatalize_t
      val sc2 = palatalize_d
in val dental_palatalizations = [sc1, sc2] end

local val sc1 = palatalize_g
      val sc2 = palatalize_k
in val velar_palatalizations = [sc1, sc2] end

local val sc1 = palatalize_l
in val sonorant_palatalizations = [sc1] end

(* proto-romance sound changes *)

local val consonantism_1 = prRom_early_cons_shift
      val vocalism = prRom_vowel_shift
      val consonantism_2 = prRom_cons_shift
      val palatalizations = dental_palatalizations @ velar_palatalizations @ sonorant_palatalizations
in val prRom_lang_shift = [consonantism_1, vocalism, consonantism_2, palatalizations]
end

(* western romance vocalism *)

(* diphthongization of low-mid [epsilon] (PrRom) *)

fun diph_epsilon_subst nuc
    = case nuc of
          Monophthong v => if v = seg_e'
                           then Diphthong (seg_i, seg_e)
                           else nuc
        | _ => nuc

val diph_epsilon_rewrite = Nucleusism diph_epsilon_subst

fun is_stressed (Syllable (onset, nuc, cod, Stressed)) = true
  | is_stressed _ = false

fun is_open_syll (Syllable (onset, nuc, cod, stress))
    = case cod of
          ZeroCoda => true
        | _ => false

fun all_true [] x = true
  | all_true (f :: fs) x = (f x) andalso (all_true fs x)

val diph_epsilon = let val syllabism = let val p = all_true [is_stressed, is_open_syll]
                                       in mkSyllabism diph_epsilon_rewrite (Predicate p) end
                       val name = "Diphthongization of [\201\155]"
                   in mkSoundChange syllabism name end

(* western romance vowel shift *)

local val sc1 = diph_epsilon
in val WRom_vowel_shift = [sc1] end

(* western romance consonantism *)

(* lenition *)

structure Lenition = struct
fun mkSubstF f onset =
    case onset of
        Onset cons => Onset (f cons)
      | OnsetM (cons1, cons2) => let val cons1' = f cons1
                                 in OnsetM (cons1', cons2) end
      | _ => onset

fun mkSyllF f syll =
    let val Syllable (on, nuc, cod, stress) = syll
        val on' = f on
    in Syllable (on', nuc, cod, stress) end

local
    fun context syll1 syll2 =
        let fun context1 syll =
                let val Syllable (on, nuc, cod, stress) = syll
                in case cod of
                       ZeroCoda => true
                     | _ => false
                end
            fun context2 syll =
                let val Syllable (on, nuc, cod, stress) = syll
                in case on of
                       Onset _ => true
                     | OnsetM _ => true
                     | _ => false
                end
        in (context1 syll1) andalso (context2 syll2) end
in
fun mkPWordF f pword =
    case pword of
        [] => pword
      | [x] => pword
      | [x, y] => if context x y
                  then [x, f y]
                  else pword
      | (x :: y :: xs) => if context x y
                          then x :: mkPWordF f ((f y) :: xs)
                          else x :: mkPWordF f (y :: xs)
end

fun mkLenition (lenite : consonant -> consonant) name =
    let val subst = mkSubstF lenite
        val syllF = mkSyllF subst
        val pWordF = mkPWordF syllF
    in SoundChange (pWordF, name) end
end

(* lenition i *)

structure LenitionI = struct
fun lenite (Consonant (Voiceless, place, Stop)) = Consonant (Voiced, place, Stop)
  | lenite (Consonant (Voiced, place, Stop)) = Consonant (Voiced, place, NonSibil)
  | lenite (Consonant (Voiceless, place, Sibilant)) = Consonant (Voiced, place, Sibilant)
  | lenite (Consonant (Voiceless, place, NonSibil)) = Consonant (Voiced, place, NonSibil)
  | lenite cons = cons

val sound_change = let val name = "Intervocalic Lenition I"
                   in Lenition.mkLenition lenite name end
end

val lenition_1 = LenitionI.sound_change

(* degemination *)

structure WRomDegemination = struct
fun subst coda =
    case coda of
        Codetta _ => ZeroCoda
      | _ => coda

fun syllabism f syll =
    let val Syllable (on, nuc, cod, stress) = syll
        val cod' = f cod
    in Syllable (on, nuc, cod', stress) end

fun context syll1 syll2 =
    let val Syllable (_, _, cod1, _) = syll1
        val Syllable (on2, _, _, _) = syll2
    in
        case (cod1, on2) of
            (Codetta cons1, Onset cons2) => if cons1 = cons2
                                            then true
                                            else
                                                (cons1 = seg_t andalso cons2 = seg_ts)
                                                orelse (cons1 = seg_l andalso cons2 = seg_ch)
          | (Codetta cons1, OnsetM (cons2, _)) => (cons1 = cons2)
          | _ => false
    end

fun pwordism f [] = []
  | pwordism f [x] = [x]
  | pwordism f (x :: y :: xs) = if context x y
                                then f x :: pwordism f (y :: xs)
                                else x :: pwordism f (y :: xs)

val sound_change =
    let val f = syllabism subst
        val g = pwordism f
        val name = "Degemination"
    in SoundChange (g, name) end
end

val degemination = WRomDegemination.sound_change

(* western romance consonantal shift *)

local val sc1 = lenition_1
      val sc2 = degemination
in val WRom_cons_shift = [sc1, sc2] end

(* western romance sound changes *)

local val vocalism = WRom_vowel_shift
      val consonantism = WRom_cons_shift
in val WRom_lang_shift = [vocalism, consonantism] end

(* old spanish vocalism *)

(* diphthongization of [epsilon] in closed syllables *)

val diph_epsilon_2 = let val syllabism = mkSyllabism diph_epsilon_rewrite (Predicate is_stressed)
                         val name = "Diphthongization of [\201\155] II"
                     in mkSoundChange syllabism name end

(* diphthongization of the low-mid back vowel *)

fun diph_open_o_subst nuc
    = case nuc of
          Monophthong v => if v = seg_o'
                           then Diphthong (seg_u, seg_e)
                           else nuc
        | _ => nuc

val diph_open_o_rewrite = Nucleusism diph_open_o_subst

val diph_open_o = let val syllabism = mkSyllabism diph_open_o_rewrite (Predicate is_stressed)
                      val name = "Diphthongization of [\201\148]"
                  in mkSoundChange syllabism name end

(* apocope *)

structure Apocope = struct

end

(* old spanish vowel shift *)

local val sc1 = diph_epsilon_2
      val sc2 = diph_open_o
in val OSp_vowel_shift = [sc1, sc2] end

(* old spanish consonantism *)

(* spirantization of [lh] *)

structure SpirantizationLL = struct
fun subst onset =
    case onset of
        Onset cons => if cons = seg_lh
                      then Onset seg_zh
                      else onset
      | _ => onset

val sound_change =
    let val rewrite = Onsetism subst
        val syllabism = mkSyllabism rewrite NoContext
        val name = "Spirantization of [\202\142]"
    in mkSoundChange syllabism name end
end

val spirantize_lh = SpirantizationLL.sound_change

(* debuccalization of [phi] *)

structure Debuccalization = struct
fun subst onset =
    case onset of
        Onset cons => if cons = seg_ph
                      then Onset seg_h
                      else onset
      | _ => onset

fun p syll =
    let val Syllable (on, nuc, cod, stress) = syll
    in case nuc of
           Diphthong (v1, v2) => not (v1 = seg_u)
         | _ => true
    end

val sound_change =
    let val syllabism =
            let val rewrite = Onsetism subst
                val pred = Predicate p
            in mkSyllabism rewrite pred end
        val name = "Debuccalization of [\201\184]"
    in mkSoundChange syllabism name end
end

val debuccalize_f = Debuccalization.sound_change

(* fronting of palatal affricates *)

structure FrontingPalatal = struct
fun subst onset =
    case onset of
        Onset cons => if cons = seg_ch
                      then Onset seg_ts
                      else onset
      | _ => onset

val rewrite = Onsetism subst

val sound_change =
    let val syllabism = mkSyllabism rewrite NoContext
        val name = "Fronting of [t\202\131]"
    in mkSoundChange syllabism name end
end

val fronting_palatal = FrontingPalatal.sound_change

(* lenition ii *)

structure LenitionII = struct
fun lenite (Consonant (Voiced, place, Stop)) = Consonant (Voiced, place, NonSibil)
  | lenite (Consonant (Voiceless, place, Affricate)) = Consonant (Voiced, place, Affricate)
  | lenite cons = cons

val sound_change = let val name = "Intervocalic Lenition II"
                   in Lenition.mkLenition lenite name end
end

val lenition_2 = LenitionII.sound_change

(* old spanish consonantal shifts *)

local val sc1 = fronting_palatal
      val sc2 = debuccalize_f
      val sc3 = lenition_2
      val sc4 = spirantize_lh
in val OSp_cons_shift = [sc1, sc2, sc3, sc4] end

(* old spanish sound changes *)

local val vocalism = OSp_vowel_shift
      val consonantism = OSp_cons_shift
in val OSp_lang_shift = [vocalism, consonantism] end

(* modern spanish vocalism *)

(* modern spanish consonantism *)

(* betacism *)

structure Betacism = struct

fun subst onset =
    case onset of
        Onset cons => if cons = seg_bh
                      then Onset seg_b
                      else onset
      | _ => onset

val rewrite = Onsetism subst

val sound_change =
    let val syllabism = mkSyllabism rewrite WordInit
        val name = "Betacism"
    in mkSoundChange syllabism name end

end

val betacism = Betacism.sound_change

(* labiodentalization *)

structure Labiodentalization = struct
fun subst onset =
    case onset of
        Onset cons => if cons = seg_ph
                      then Onset seg_f
                      else onset
      | _ => onset

val sound_change =
    let val rewrite = Onsetism subst
        val syllabism = mkSyllabism rewrite NoContext
        val name = "Labiodentalization of [\201\184]"
    in mkSoundChange syllabism name end
end

val labiodentalization = Labiodentalization.sound_change

(* deaffrication of [dg] *)

fun deaffric_dg_subst onset =
    case onset of
        Onset cons => if cons = seg_dg
                      then Onset seg_zh
                      else onset
      | _ => onset

val deaffric_dg_rewrite = Onsetism deaffric_dg_subst

val deaffric_dg = let val syllabism = mkSyllabism deaffric_dg_rewrite NoContext
                      val name = "Deaffrication of [d\202\146]"
                  in mkSoundChange syllabism name end

(* deaffrication of [ts] and [dz] *)

structure DeaffricationDent = struct
fun deaffric_dent_subst cons =
    case cons of
        Consonant (voice, Dental, Affricate) => Consonant (voice, Dental, Sibilant)
      | _ => cons

fun deaffric_dent_syll f syll =
    let val Syllable (on, nuc, cod, stress) = syll
        val on' = case on of
                      Onset cons => Onset (f cons)
                    | _ => on
        val cod' = case cod of
                       Codetta cons => Codetta (f cons)
                     | _ => cod
    in Syllable (on', nuc, cod', stress) end

val deaffric_dent_syllabism =
    let val syllF = deaffric_dent_syll deaffric_dent_subst
    in Syllabism (syllF, NoContext) end

val deaffric_dent =
    let val syllabism = deaffric_dent_syllabism
        val name = "Deaffrication of Dental Affricates"
    in mkSoundChange syllabism name end
end

val deaffric_dent = DeaffricationDent.deaffric_dent

(* devoicing of sibilants *)

structure DevoiceSibil = struct
fun devoice_sibil_seg cons =
    case cons of
        Consonant (Voiced, place, Sibilant) => Consonant (Voiceless, place, Sibilant)
      | _ => cons

fun devoice_sibil_syll f syll =
    let val Syllable (on, nuc, cod, stress) = syll
        val on' = case on of
                      Onset cons => Onset (f cons)
                    | _ => on
        val cod' = case cod of
                       Codetta cons => Codetta (f cons)
                     | _ => cod
    in Syllable (on', nuc, cod', stress) end

val devoice_sibil_syllabism =
    let val syllF = devoice_sibil_syll devoice_sibil_seg
    in Syllabism (syllF, NoContext) end

val devoice_sibil = let val syllabism = devoice_sibil_syllabism
                        val name = "Devoicing of Sibilants"
                    in mkSoundChange syllabism name end
end

val devoice_sibil = DevoiceSibil.devoice_sibil

(* desibilation of dental sibilants *)

structure Desibilation = struct
fun subst cons =
    case cons of
        Consonant (voice, Dental, Sibilant) => Consonant (voice, Dental, NonSibil)
      | _ => cons

fun syllF f syll =
    let val Syllable (on, nuc, cod, stress) = syll
        val on' = case on of
                      Onset cons => Onset (f cons)
                    | _ => on
        val cod' = case cod of
                       Codetta cons => Codetta (f cons)
                     | _ => cod
    in Syllable (on', nuc, cod', stress) end

val syllabism = Syllabism (syllF subst, NoContext)

val sound_change = let val name = "Desibilation of Dental Sibilants"
                   in mkSoundChange syllabism name end
end

val desibil_dent = Desibilation.sound_change

(* retraction of [sh] *)

fun retraction_sh_subst onset
    = case onset of
          Onset cons => if cons = seg_sh
                        then Onset seg_x
                        else onset
        | _ => onset

val retraction_sh_rewrite = Onsetism retraction_sh_subst

val retraction_sh = let val syllabism = mkSyllabism retraction_sh_rewrite NoContext
                        val name = "Retraction of [\202\131]"
                    in mkSoundChange syllabism name end

(* modern spanish consonantal shift *)

local val sc1 = betacism
      val sc2 = deasp
      val sc3 = labiodentalization
in val Es_cons_shift = [sc1, sc2, sc3] end

local val sc1 = deaffric_dg
      val sc2 = deaffric_dent
      val sc3 = devoice_sibil
      val sc4 = desibil_dent
      val sc5 = retraction_sh
in val Es_sibil_shift = [sc1, sc2, sc3, sc4, sc5] end

(* modern spanish sound changes *)

val Es_lang_shift = [Es_cons_shift, Es_sibil_shift]

(* gramática histórica española *)

val historia = [ prRom_lang_shift
               , WRom_lang_shift
               , OSp_lang_shift
               , Es_lang_shift ]
