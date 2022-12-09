signature SYLLABISM = sig
    datatype rewrite = Onsetism of (Onset.t -> Onset.t)
                     | Nuxism of (Nucleus.t -> Nucleus.t)
                     | Codism of (Coda.t -> Coda.t)

    datatype context = NoContext
                     | Predicate of (Syll.t -> bool)
                     | WordInit
                     | WordFinal

    type t = rewrite * context

    val eval : rewrite -> Syll.t -> Syll.t

    val lift' : (Syll.t -> Syll.t) -> context -> PWord.t -> PWord.t
    val lift : t -> PWord.t -> PWord.t
end

structure Syllabism : SYLLABISM = struct
    datatype rewrite = Onsetism of (Onset.t -> Onset.t)
                     | Nuxism of (Nucleus.t -> Nucleus.t)
                     | Codism of (Coda.t -> Coda.t)

    datatype context = NoContext
                     | Predicate of (Syll.t -> bool)
                     | WordInit
                     | WordFinal

    type t = rewrite * context

    fun apply_first f [] = []
      | apply_first f (x :: xs) = f x :: xs

    fun apply_last f l = let val l' = apply_first f (rev l)
                         in rev l' end

    fun apply_only pred f [] = []
      | apply_only pred f (x :: xs)
        = if pred x
          then f x :: apply_only pred f xs
          else x :: apply_only pred f xs

    fun eval rewrite syll =
        let val Syll.Syll (onset, nuc, coda, stress) = syll
        in case rewrite
            of Onsetism f => Syll.Syll (f onset, nuc, coda, stress)
             | Nuxism f => Syll.Syll (onset, f nuc, coda, stress)
             | Codism f => Syll.Syll (onset, nuc, f coda, stress)
        end

    fun lift' f context
        = case context
           of NoContext => map f
            | Predicate pred => apply_only pred f
            | WordInit => apply_first f
            | WordFinal => apply_last f

    fun lift (rewrite, context) =
        let val f = eval rewrite
        in lift' f context end
end

signature SYLLABISM_L = sig
    type t = Syllabism.rewrite list * Syllabism.context

    val eval : Syllabism.rewrite list -> Syll.t -> Syll.t

    val lift : t -> PWord.t -> PWord.t
end

structure Syllabism_L : SYLLABISM_L = struct
    type t = Syllabism.rewrite list * Syllabism.context

    fun eval [] syll = syll
      | eval (x :: xs) syll =
        let val f = Syllabism.eval x
            val syll' = f syll
        in eval xs syll' end

    fun lift (rewrite_l, context) pword
        = let val f = eval rewrite_l
          in Syllabism.lift' f context pword end
end

signature DISYLLABISM = sig
    type rewrite = (Syll.t -> Syll.t) * (Syll.t -> Syll.t)
    type context = (Syll.t -> bool) * (Syll.t -> bool)
    type t = rewrite * context

    val applicable : context -> (Syll.t * Syll.t) -> bool
    val apply : rewrite -> (Syll.t * Syll.t) -> (Syll.t * Syll.t)

    val lift : t -> PWord.t -> PWord.t
end

structure Disyllabism : DISYLLABISM = struct
    type rewrite = (Syll.t -> Syll.t) * (Syll.t -> Syll.t)
    type context = (Syll.t -> bool) * (Syll.t -> bool)
    type t = rewrite * context

    fun applicable (pred1, pred2) (syll1, syll2) =
        let val context1 = pred1 syll1
            val context2 = pred2 syll2
        in context1 andalso context2 end

    fun apply (f1, f2) (syll1, syll2) = (f1 syll1, f2 syll2)

    fun lift (disyllabism as (rewrite, context)) pword =
        case pword
         of [] => []
          | [x] => pword
          | (x :: y :: xs) => if applicable context (x, y)
                              then let val (x', y') = apply rewrite (x, y)
                                   in x' :: lift disyllabism (y' :: xs) end
                              else x :: lift disyllabism (y :: xs)
end

signature RULE = sig
    datatype rewrite = Syllabism of Syllabism.t
                     | Syllabism_L of Syllabism_L.t
                     | Disyllabism of Disyllabism.t

    datatype t = Rule of string * (PWord.t -> PWord.t)

    val eval : rewrite -> (PWord.t -> PWord.t)
end

structure Rule : RULE = struct
    datatype rewrite = Syllabism of Syllabism.t
                     | Syllabism_L of Syllabism_L.t
                     | Disyllabism of Disyllabism.t

    datatype t = Rule of string * (PWord.t -> PWord.t)

    fun eval (Syllabism syllabism) = Syllabism.lift syllabism
      | eval (Syllabism_L syllabism_l) = Syllabism_L.lift syllabism_l
      | eval (Disyllabism disyllabism) = Disyllabism.lift disyllabism
end
