signature CHANGE = sig
    val change : PWord.t -> PWord.t
    val name : string
    val rule : Rule.t
end

signature VOCALISM = sig
    include CHANGE
    val vocalism : Vowel.t -> Vowel.t 
    val rewrite : Nucleus.t -> Nucleus.t
end

signature CONSONANTISM = sig
    include CHANGE
    val consonantism : Consonant.t -> Consonant.t
end

functor PalatalizationFun 
(Config : sig
     val input : Consonant.t
     val output : Consonant.t
     val nuc_pred : Nucleus.t -> bool
 end) : sig
    include CHANGE
    val consonantism : Consonant.t -> Consonant.t
    val subst : Onset.t -> Onset.t
    val pred : Syll.t -> bool
end = struct
    fun consonantism cons = if cons = Config.input
                            then Config.output
                            else cons

    fun subst onset =
        case onset of
            Onset.Singleton cons => 
            Onset.Singleton (consonantism cons)
          | _ => onset

    fun subst' nuc = 
        case nuc of
            Nucleus.Diphthong (v1, v2) => Nucleus.Monophthong v2
          | _ => nuc

    fun pred (Syll.Syll (onset, nuc, _, _)) = 
        (Config.nuc_pred nuc) 
        andalso (case onset 
                  of Onset.Singleton cons => cons = Config.input
                  | Onset.Complex (cons1, cons2) => cons1 = Config.input
                  | _ => false)

    val change = let val rewrite = [Syllabism.Onsetism subst, Syllabism.Nuxism subst']
                     val syllabism = Rule.Syllabism_L (rewrite, Syllabism.Predicate pred)
                 in Rule.eval syllabism end
    val name = "Palatalization of ["
               ^ Consonant.toStr Config.input ^ "]"
    val rule = Rule.T (name, change)
end

functor DiphthongizationFun
(Config : sig
    val input : Vowel.t
    val output : Vowel.t * Vowel.t
    val pred : Syll.t -> bool
end) : sig
    include CHANGE
    val subst : Nucleus.t -> Nucleus.t
end = struct
    fun subst nuc = 
        case nuc 
         of Nucleus.Monophthong v => if v = Config.input
                             then Nucleus.Diphthong Config.output
                             else nuc
          | _ => nuc

    val change = let val rewrite = Syllabism.Nuxism subst
                     val syllabism = Rule.Syllabism (rewrite, Syllabism.Predicate Config.pred)
                 in Rule.eval syllabism end
    val name = "Diphthongization of ["
               ^ Vowel.toStr Config.input ^ "]"
    val rule = Rule.T (name, change)
end

functor LenitionFun
(Config : sig
     val lenite : Consonant.t -> Consonant.t
end) : sig
    include CHANGE
    val subst : Onset.t -> Onset.t
    val rewrite : Disyllabism.rewrite
    val context : Disyllabism.context
end = struct
    fun subst (Onset.Singleton cons) = Onset.Singleton (Config.lenite cons)
      | subst (Onset.Complex (cons1, cons2)) = Onset.Complex (Config.lenite cons1, cons2)
      | subst onset = onset

    val rewrite = let fun pred (syll : Syll.t) = syll
                      val next = (Syllabism.eval o Syllabism.Onsetism) subst
                  in (pred, next) end

    fun context (pred, next) = let fun context_pred (Syll.Syll (_, _, Coda.ZeroCoda, _)) = true
                                  | context_pred _ = false
                                fun context_next (Syll.Syll (onset, _, _, _)) =
                                    case onset 
                                     of Onset.Singleton cons => true
                                      | Onset.Complex (cons1, cons2) => true
                                      (* think twice about this part *)
                                      | _ => false
                            in (context_pred pred) andalso (context_next next) end

    val change = let val disyllabism = Rule.Disyllabism (rewrite, context)
                 in Rule.eval disyllabism end
    val name = "Lenition"
    val rule = Rule.T (name, change)
end

functor DegeminationFun
(Config : sig
    val pred : (Consonant.t * Consonant.t) -> bool
end) : sig
    include CHANGE
    val subst : Coda.t -> Coda.t
    val rewrite : Disyllabism.rewrite
    val context : Disyllabism.context
end = struct
    fun subst coda = 
        case coda 
         of Coda.Coda cons => Coda.ZeroCoda
          | _ => coda

    val rewrite = let val ante = (Syllabism.eval o Syllabism.Codism) subst
                      val post : Syll.t -> Syll.t = fn x => x
                  in (ante, post) end

    fun context ((Syll.Syll (_, _, coda, _)), (Syll.Syll (onset, _, _, _)))
        = case (coda, onset) 
           of ((Coda.Coda cons1), (Onset.Singleton cons2)) => Config.pred (cons1, cons2)
           | ((Coda.Coda cons1), (Onset.Complex (cons2, _))) => Config.pred (cons1, cons2)
           (* think twice about this *)
           | _ => false

    val change = let val disyllabism = Rule.Disyllabism (rewrite, context)
                 in Rule.eval disyllabism end
    val name = "Degemination"
    val rule = Rule.T (name, change)
end
