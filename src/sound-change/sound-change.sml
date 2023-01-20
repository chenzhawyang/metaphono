structure Changes = struct

open Vowel Consonant Onset Nucleus Coda Syll

structure Monophthongization : CHANGE = struct
    fun subst (diph as (Diphthong (v1, v2))) =
        (* check these if anything goes wrong *)
        (case (v1, v2) of
              ((Vowel (Mid, Back)), (Vowel (High, Front))) => LongVowel (Vowel (Mid, Front))
           |  ((Vowel (Low, Central)), (Vowel (High, Back))) => LongVowel (Vowel (Mid, Back))
           |  ((Vowel (Low, Central)), (Vowel (High, Front))) => Monophthong (Vowel (Mid, Front))
           |  _ => diph)
      | subst nuc = nuc

    val change = let val rewrite = Syllabism.Nuxism subst
                     val syllabism = Rule.Syllabism (rewrite, Syllabism.NoContext)
                 in Rule.eval syllabism end
    val name = "Monophthongization"
    val rule = Rule.T (name, change)
end

structure LossOfQuant : CHANGE = struct
    fun vocalism v = 
        case v of
            Vowel (High, cent) => Vowel (HighMid, cent)
          | Vowel (Mid, cent) => Vowel (LowMid, cent)
          | _ => v

    fun subst nuc = 
        case nuc of
            LongVowel v => Monophthong v
          | Monophthong v => Monophthong (vocalism v)
          | Diphthong (v1, v2) => if Nucleus.is_yod v1
                                  then Diphthong (v1, vocalism v2)
                                  else nuc

    val change = let val rewrite = Syllabism.Nuxism subst
                     val syllabism = Rule.Syllabism (rewrite, Syllabism.NoContext)
                 in Rule.eval syllabism end
    val name = "Loss of Vowel Quantity"
    val rule = Rule.T (name, change)
end

structure GreatMerger : CHANGE = struct
    fun vocalism v = 
        case v of
            Vowel (HighMid, cent) => Vowel (Mid, cent)
          | _ => v

    fun subst nuc = 
        case nuc of
            Monophthong v => Monophthong (vocalism v)
          | Diphthong (v1, v2) => if Nucleus.is_yod v1
                                  then Diphthong (v1, vocalism v2)
                                  else nuc
          | _ => nuc

    val change = let val rewrite = Syllabism.Nuxism subst
                     val syllabism = Rule.Syllabism (rewrite, Syllabism.NoContext)
                 in Rule.eval syllabism end
    val name = "Great Merger"
    val rule = Rule.T (name, change)
end

structure AtonicMerger : CHANGE = struct
    fun vocalism v =
        case v of
            Vowel (LowMid, cent) => Vowel (Mid, cent)
          | _ => v

    fun subst nuc = 
        case nuc of
            Monophthong v => Monophthong (vocalism v)
          (* if anything goes wrong, it's probably here *)
          | Diphthong (v1, v2) => if Nucleus.is_yod v1
                                  then Diphthong (v1, vocalism v2)
                                  else nuc
          | _  => nuc

    val change = let val rewrite = Syllabism.Nuxism subst 
                     val not_stressed = not o Syll.is_stressed
                     val syllabism = Rule.Syllabism (rewrite, Syllabism.Predicate not_stressed)
                 in Rule.eval syllabism end
    val name = "Atonic Merger"
    val rule = Rule.T (name, change)
end

structure FinalVowelReduction : VOCALISM = struct
    fun vocalism v = 
        case v 
         of (Vowel (height, Front)) => (Vowel (Mid, Front))
          | (Vowel (height, Back)) => (Vowel (Mid, Back))
          | _ => v

    fun subst (Monophthong v) = Monophthong (vocalism v)
      | subst nuc = nuc

    val change = let val rewrite = Syllabism.Nuxism subst
                     val syllabism = Rule.Syllabism (rewrite, Syllabism.WordFinal)
                 in Rule.eval syllabism end
    val name = "Final Vowel Reduction"
    val rule = Rule.T (name, change)
end

structure ElideWordFinalConsonant 
: sig
    val to_subst : Consonant.t -> Coda.t -> Coda.t
    val to_change : (Coda.t -> Coda.t) -> (PWord.t -> PWord.t)
    val mk : Consonant.t -> Rule.t
end = struct
    fun to_subst elided coda =
        case coda of
            Coda cons => if cons = elided
                         then ZeroCoda
                         else coda
          | _ => coda
                     
    fun to_change subst = 
        let val rewrite = Syllabism.Codism subst
            val syllabism = Rule.Syllabism (rewrite, Syllabism.WordFinal)
        in Rule.eval syllabism end

    fun mk cons = let val change = (to_change o to_subst) cons
                      val name = "Elision of Word-Final [" 
                                 ^ Consonant.toStr cons ^ "]"
                  in Rule.T (name, change) end
end

structure Fortition 
: sig
    val to_subst : (Consonant.t * Consonant.t) -> Onset.t -> Onset.t
    val to_change : (Onset.t -> Onset.t) -> (PWord.t -> PWord.t)
    val mk : (Consonant.t * Consonant.t)-> Rule.t
end = struct
    fun to_subst (input, output) onset = 
        case onset of
            Singleton cons => if cons = input
                              then Singleton output
                              else onset
          | _ => onset

    fun to_change subst =
        let val rewrite = Syllabism.Onsetism subst
            val syllabism = Rule.Syllabism (rewrite, Syllabism.NoContext)
        in Rule.eval syllabism end

    fun mk (input, output)
        = let val change = (to_change o to_subst) (input, output)
              val name = "Forition of ["
                         ^ Consonant.toStr input ^ "]"
          in Rule.T (name, change) end
end

structure Deaspiration : CHANGE = struct
    fun subst onset =
        case onset of
            Singleton cons => if cons = Seg.h
                              then ZeroOnset
                              else onset
          | _ => onset

    val change = let val rewrite = Syllabism.Onsetism subst
                     val syllabism = Rule.Syllabism (rewrite, Syllabism.NoContext)
                 in Rule.eval syllabism end
    val name = "Deaspiration of [h]"
    val rule = Rule.T (name, change)
end

structure Prothesis : sig
    include CHANGE
    val proclitic : Syll.t
    val pred : PWord.t -> bool
    val prothesize : Syll.t -> (Syll.t * Syll.t)
end = struct
    val proclitic = Syll.mk ([], [Seg.i], [Seg.s])

    fun pred [] = false
      | pred (x :: xs) = 
        let val Syll (onset, _, _, _) = x
        in 
            case onset of
                Preinit (cons1, cons2) => (cons1 = Seg.s)
             | PreinitCompl (cons1, cons2, cons3) => (cons1 = Seg.s)
             | _ => false
        end

    fun prothesize syll =
        let val Syll (onset, nuc, coda, stress) = syll
            val onset' = 
                case onset of
                    Preinit (cons1, cons2) 
                    => if cons1 = Seg.s
                       then Singleton cons2
                       else onset
                  | PreinitCompl (cons1, cons2, cons3) 
                    => if cons1 = Seg.s
                       then Preinit (cons2, cons3)
                       else onset
                  | _ => onset
            val syll' = Syll (onset', nuc, coda, stress)
        in (proclitic, syll') end

    fun change [] = []
     |  change (pword as (x :: xs)) =
        if pred pword
        then 
            let val (proclitic, x') = prothesize x
            in proclitic :: x' :: xs end
        else pword

    val name = "Prothesis"
    val rule = Rule.T (name, change)
end

structure T_Palatalization =
    PalatalizationFun
        (struct
            val input = Seg.t
            val output = Seg.ts
            val nuc_pred = Nucleus.has_yod
          end)

structure D_Palatalization =
    PalatalizationFun
        (struct
            val input = Seg.d
            val output = Seg.dg
            val nuc_pred = Nucleus.has_yod
          end)

local fun pred (Monophthong v) 
          = (case v 
             of Vowel (height, Front) => true
              | _ => false)
        | pred (Diphthong (v1, v2)) = Nucleus.is_yod v1
        | pred _ = false
in
    structure K_Palatalization =
        PalatalizationFun
            (struct
                val input = Seg.k
                val output = Seg.ch
                val nuc_pred = pred
              end)                  
    structure G_Palatalization =
        PalatalizationFun
            (struct
                val input = Seg.g
                val output = Seg.dg
                val nuc_pred = pred
              end)                  
end

structure LateralPalatalization = 
    PalatalizationFun 
        (struct
            val input = Seg.l
            val output = Seg.lh
            val nuc_pred = Nucleus.has_yod
        end)

structure NasalPalatalization =
    PalatalizationFun
        (struct
            val input = Seg.n
            val output = Seg.gn
            val nuc_pred = Nucleus.has_yod
          end)

structure E_Diphthongization =
    DiphthongizationFun
        (struct 
            val input = Seg.e'
            val output = (Seg.i, Seg.e)
            fun pred (Syll (_, _, ZeroCoda, Stressed)) = true
              | pred _ = false
          end)

structure Lenition_I = 
    let fun lenite (Consonant (Voiceless, place, Stop)) = Consonant (Voiced, place, Stop)
          (* you might want to think about this *)
          | lenite (Consonant (Voiced, place, Stop)) = Consonant (Voiced, place, NonSibil)
          | lenite (Consonant (Voiceless, place, Sibilant)) = Consonant (Voiced, place, Sibilant)
          | lenite (Consonant (Voiceless, place, NonSibil)) = Consonant (Voiced, place, NonSibil)
          | lenite cons = cons
    in LenitionFun (val lenite = lenite) end

structure Degemination_I =
    let fun pred ((Consonant (Voiced, Dental, Lateral)), (Consonant (Voiced, Dental, Lateral))) 
            = false
          | pred ((Consonant (Voiced, Dental, Trill)), (Consonant (Voiced, Dental, Trill))) = false
          | pred ((Consonant (Voiced, Dental, Nasal)), (Consonant (Voiced, Dental, Nasal))) = false
          | pred ((Consonant (Voiceless, Dental, Stop)), (Consonant (Voiceless, Dental, Affricate))) = true
          | pred ((Consonant (Voiceless, Velar, Stop)), (Consonant (Voiceless, Palatal, Affricate))) = true
          | pred (cons1, cons2) = (cons1 = cons2)
    in DegeminationFun (val pred = pred) end

structure E_Diphthongization_II = 
    DiphthongizationFun 
        (struct 
            val input = Seg.e'
            val output = (Seg.i, Seg.e)
            val pred = fn _ => true
          end) 

structure O_Diphthongization =
    DiphthongizationFun
        (struct 
            val input = Seg.o'
            val output = (Seg.u, Seg.e)
            val pred = fn _ => true 
          end)

structure Spirantization : CONSONANTISM = struct
    fun consonantism cons = if cons = Seg.lh
                            then Seg.zh
                            else cons

    fun subst (Singleton cons) = Singleton (consonantism cons)
      | subst onset = onset

    val change = let val rewrite = Syllabism.Onsetism subst
                     val syllabism = Rule.Syllabism (rewrite, Syllabism.NoContext)
                 in Rule.eval syllabism end
    val name = "Spirantization of [ʎ]"
    val rule = Rule.T (name, change)
end

structure Debuccalization : CONSONANTISM = struct
    fun consonantism cons = if cons = Seg.ph
                            then Seg.h
                            else cons

    fun subst (Singleton cons) = Singleton (consonantism cons)
      | subst onset = onset

    fun pred (Syll (_, nuc, _, _)) = 
        case nuc 
         of Diphthong (v1, v2) => not (v1 = Seg.u)
          | _ => true

    val change = let val rewrite = Syllabism.Onsetism subst
                     val syllabism = Rule.Syllabism (rewrite, Syllabism.Predicate pred)
                 in Rule.eval syllabism end
    val name = "Debuccalization of [ɸ]"
    val rule = Rule.T (name, change)
end

structure Fronting : CONSONANTISM = struct
    fun consonantism cons = if cons = Seg.ch
                            then Seg.ts
                            else cons

    fun subst (Singleton cons) = Singleton (consonantism cons)
      | subst onset = onset

    val change = let val rewrite = Syllabism.Onsetism subst
                     val syllabism = Rule.Syllabism (rewrite, Syllabism.NoContext)
                 in Rule.eval syllabism end
    val name = "Fronting of [tʃ]"
    val rule = Rule.T (name, change)
end

structure Lenition_II = 
    let fun lenite (Consonant (Voiced, place, Stop)) = Consonant (Voiced, place, NonSibil)
          | lenite (Consonant (Voiceless, place, Affricate)) = Consonant (Voiced, place, Affricate)
          | lenite cons = cons
    in LenitionFun (val lenite = lenite) end

structure Betacism : CONSONANTISM = struct
    fun consonantism cons = if cons = Seg.bh
                            then Seg.b
                            else cons

    fun subst (Singleton cons) = Singleton (consonantism cons)
      | subst onset = onset

    val change = let val rewrite = Syllabism.Onsetism subst
                     val syllabism = Rule.Syllabism (rewrite, Syllabism.WordInit)
                 in Rule.eval syllabism end
    val name = "Betacism"
    val rule = Rule.T (name, change)
end

structure Labiodentalization : CONSONANTISM = struct
    fun consonantism cons = if cons = Seg.ph
                            then Seg.f
                            else cons

    fun subst (Singleton cons) = Singleton (consonantism cons)
      | subst onset = onset

    val change = let val rewrite = Syllabism.Onsetism subst
                     val syllabism = Rule.Syllabism (rewrite, Syllabism.NoContext)
                 in Rule.eval syllabism end
    val name = "Labiodentalization"
    val rule = Rule.T (name, change)
end

structure Deaffrication_Palatal : CONSONANTISM = struct
    fun consonantism cons = if cons = Seg.dg
                            then Seg.zh
                            else cons

    fun subst (Singleton cons) = Singleton (consonantism cons)
      | subst onset = onset

    val change = let val rewrite = Syllabism.Onsetism subst
                     val syllabism = Rule.Syllabism (rewrite, Syllabism.NoContext)
                 in Rule.eval syllabism end
    val name = "Deaffrication of [dʒ]"
    val rule = Rule.T (name, change)
end

structure Deaffrication_Dental : CONSONANTISM = struct
    fun consonantism (Consonant (voice, Dental, Affricate)) = Consonant (voice, Dental, Sibilant)
      | consonantism cons = cons

    fun subst (Singleton cons) = Singleton (consonantism cons)
      | subst onset = onset

    fun subst' (Coda cons) = Coda (consonantism cons)
      | subst' coda = coda

    val change = let val rewrite = [ Syllabism.Onsetism subst
                                   , Syllabism.Codism subst' ]
                     val syllabism = Rule.Syllabism_L (rewrite, Syllabism.NoContext)
                 in Rule.eval syllabism end
    val name = "Deaffrication of Dental Affricates"
    val rule = Rule.T (name, change)
end

structure DevoicingSibilant : CONSONANTISM = struct
    fun consonantism (Consonant (Voiced, place, Sibilant)) = Consonant (Voiceless, place, Sibilant)
      | consonantism cons = cons

    fun subst (Singleton cons) = Singleton (consonantism cons)
      | subst onset = onset

    fun subst' (Coda cons) = Coda (consonantism cons)
      | subst' coda = coda

    val change = let val rewrite = [ Syllabism.Onsetism subst
                                   , Syllabism.Codism subst' ]
                     val syllabism = Rule.Syllabism_L (rewrite, Syllabism.NoContext)
                 in Rule.eval syllabism end
    val name = "Devoicing of Sibilants"
    val rule = Rule.T (name, change)
end

structure Desibilation : CONSONANTISM = struct
    fun consonantism (Consonant (voice, Dental, Sibilant)) = Consonant (voice, Dental, NonSibil)
      | consonantism cons = cons

    fun subst (Singleton cons) = Singleton (consonantism cons)
      | subst onset = onset

    fun subst' (Coda cons) = Coda (consonantism cons)
      | subst' coda = coda

    val change = let val rewrite = [ Syllabism.Onsetism subst
                                   , Syllabism.Codism subst' ]
                     val syllabism = Rule.Syllabism_L (rewrite, Syllabism.NoContext)
                 in Rule.eval syllabism end
    val name = "Desibilation of Dental Sibilants"
    val rule = Rule.T (name, change)
end

structure Retraction : CONSONANTISM = struct
    fun consonantism cons = if cons = Seg.sh
                            then Seg.x
                            else cons

    fun subst (Singleton cons) = Singleton (consonantism cons)
      | subst onset = onset

    val change = let val rewrite = Syllabism.Onsetism subst
                     val syllabism = Rule.Syllabism (rewrite, Syllabism.NoContext)
                 in Rule.eval syllabism end
    val name = "Retraction of [ʃ]"
    val rule = Rule.T (name, change)
end
 
(* above were the sound changes written during thanksgiving in cambridge, england *)

structure Metaphony = struct
    (* this is the most general case *)
    fun vocalism v =
        case v
         of (Vowel (Mid, cent)) => Vowel (High, cent)
          | (Vowel (LowMid, cent)) => Vowel (Mid, cent)
          | (Vowel (Low, Central)) => Vowel (Mid, Front)
          | _ => v

    fun subst (Monophthong v) = Monophthong (vocalism v)
      | subst nuc = nuc

    val rewrite = let val ante = (Syllabism.eval o Syllabism.Nuxism) subst
                      val post : Syll.t -> Syll.t = (fn x => x)
                  in (ante, post) end
    
    fun context (syll1, syll2) = 
        let fun context1 (Syll (_, nuc, _, Stressed)) = 
                (case nuc
                  of (Monophthong _) => true
                  |  _ => false)
              | context1 _ = false
            fun context2 (Syll (_, nuc, _, _)) = (Nucleus.has_yod nuc)
        in (context1 syll1 andalso context2 syll2) end

    val change = let val disyllabism = Rule.Disyllabism (rewrite, context)
                 in Rule.eval disyllabism end
    val name = "Metaphony"
    val rule = Rule.T (name, change)
end

(* rewrite this later *)
structure Apocope : sig
    include CHANGE
    val rewrite : (Syll.t * Syll.t) -> Syll.t
    val pred : PWord.t -> bool
end = struct
    exception BadFinal

    fun rewrite (Syll (onset1, nuc1, ZeroCoda, stress1), Syll (Singleton cons, _, _, _)) 
        = Syll (onset1, nuc1, Coda cons, stress1)
      | rewrite _ = raise BadFinal

    fun pred [] = false
      | pred [x] = false
      | pred pword = let val (ult :: penult :: _) = rev pword
                         val (Syll (_, _, coda, _)) = penult
                         val (Syll (onset, nuc, coda', _)) = ult
                     in 
                         case nuc 
                          of Monophthong v => 
                             if v = Seg.e
                             then (case (coda, onset, coda')
                                    of (ZeroCoda, Singleton _, ZeroCoda) => true
                                     | _ => false)
                             else false
                           | _ => false
                     end

    fun change pword = if pred pword
                       then 
                           case pword of
                               [] => pword
                             | [x] => pword
                             | [x, y] => [rewrite (x, y)]
                             | l => let val (ult :: penult :: xs) = rev pword
                                        val ult' = rewrite (penult, ult)
                                    in rev (ult :: xs) end
                       else pword

    val name = "Apocope"
    val rule = Rule.T (name, change)
end

(* requires double checking *)
structure Degemination_L_N : CONSONANTISM = struct
    fun consonantism (Consonant (Voiced, Dental, Lateral)) = Seg.lh
      | consonantism (Consonant (Voiced, Dental, Nasal)) = Seg.gn
      | consonantism cons = cons

    fun subst (Coda _) = ZeroCoda
      | subst coda = coda

    fun subst' (Singleton cons) = Singleton (consonantism cons)
      | subst' onset = onset

    val rewrite = let val ante = (Syllabism.eval o Syllabism.Codism) subst
                      val post = (Syllabism.eval o Syllabism.Onsetism) subst'
                  in (ante, post) end

    fun context (syll1, syll2) = 
        let val (Syll (_, _, coda1, _)) = syll1
            val (Syll (onset2, _, _, _)) = syll2
        in 
            case (coda1, onset2)
            of (Coda cons1, Singleton cons2) => (cons1 = cons2)
             | _ => false
        end

    val change = let val disyllabism = Rule.Disyllabism (rewrite, context)
                 in Rule.eval disyllabism end
    val name = "Degemination"
    val rule = Rule.T (name, change)
end

structure Degemination_R (* : CONSONANTISM *) = struct
    
end

end
