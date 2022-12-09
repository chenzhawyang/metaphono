signature ONSET = sig
    exception BadOnset

    datatype t = ZeroOnset
               | Singleton of Consonant.t
               | Complex of Consonant.t * Consonant.t
               | Preinit of Consonant.t * Consonant.t
               | PreinitCompl of Consonant.t * Consonant.t * Consonant.t

    val mk : Consonant.t list -> t

    val toStr : t -> string
end

structure Onset : ONSET = struct
    exception BadOnset

    datatype t = ZeroOnset
               | Singleton of Consonant.t
               | Complex of Consonant.t * Consonant.t
               | Preinit of Consonant.t * Consonant.t
               | PreinitCompl of Consonant.t * Consonant.t * Consonant.t

    local open Consonant in 
        fun is_medial cons = (cons = Seg.r) orelse (cons = Seg.l)
        fun is_stop (Consonant (_, _, Stop)) = true
          | is_stop _ = false
        fun is_ph cons = (cons = Seg.ph)
        fun is_dental_stop (Consonant (_, Dental, Stop)) = true
          | is_dental_stop _ = false
        fun is_s cons = (cons = Seg.s)
    end

    fun is_complex (cons1, cons2) =
        if (is_stop cons1 orelse is_ph cons1) andalso (is_medial cons2)
        then if (cons2 = Seg.r)
            then true
            else not (is_dental_stop cons1)
        else false

    fun is_preinit (cons1, cons2) = (is_s cons1) andalso (is_stop cons2)

    fun mk [] = ZeroOnset
      | mk [x] = Singleton x
      | mk [x, y] = if is_complex (x, y)
                    then Complex (x, y)
                    else 
                        if is_preinit (x, y)
                        then Preinit (x, y)
                        else raise BadOnset
      | mk [x, y, z] = if (is_s x) andalso (is_stop y) andalso (z = Seg.r)
                       then PreinitCompl (x, y, z)
                       else raise BadOnset
      | mk _ = raise BadOnset

    fun toStr ZeroOnset = ""
      | toStr (Singleton cons) = Consonant.toStr cons
      | toStr (Complex (cons1, cons2)) = Consonant.toStr cons1 ^ Consonant.toStr cons2
      | toStr (Preinit (cons1, cons2)) = Consonant.toStr cons1 ^ Consonant.toStr cons2
      | toStr (PreinitCompl (cons1, cons2, cons3)) = Consonant.toStr cons1 ^ Consonant.toStr cons2 ^ Consonant.toStr cons3
end

signature NUCLEUS = sig
    exception BadNuc

    datatype t = Monophthong of Vowel.t
               | Diphthong of Vowel.t * Vowel.t
               | LongVowel of Vowel.t

    val mk : Vowel.t list -> t

    val toStr : t -> string
end

structure Nucleus : NUCLEUS = struct
    exception BadNuc

    datatype t = Monophthong of Vowel.t
               | Diphthong of Vowel.t * Vowel.t
               | LongVowel of Vowel.t

    fun mk [] = raise BadNuc
      | mk [x] = Monophthong x
      | mk [x, y] = if (x = y)
                    then LongVowel x
                    else Diphthong (x, y)
      | mk _ = raise BadNuc

    fun is_glide v = (v = Seg.i orelse v = Seg.u)

    fun toStr (Monophthong v) = Vowel.toStr v
      | toStr (Diphthong (v1, v2)) 
        = if is_glide v1
          then Vowel.toStr v1 ^ "̯" ^ Vowel.toStr v2
          else Vowel.toStr v1 ^ Vowel.toStr v2 ^ "̯"
      | toStr (LongVowel v) = Vowel.toStr v ^ "ː"
end

signature CODA = sig
    exception BadCoda

    datatype t = ZeroCoda
               | Coda of Consonant.t
               | PostCoda of Consonant.t * Consonant.t
               | PostCodaCompl of Consonant.t * Consonant.t * Consonant.t

    val mk : Consonant.t list -> t

    val toStr : t -> string
end

structure Coda : CODA = struct
    exception BadCoda

    datatype t = ZeroCoda
               | Coda of Consonant.t
               | PostCoda of Consonant.t * Consonant.t
               | PostCodaCompl of Consonant.t * Consonant.t * Consonant.t

    fun mk [] = ZeroCoda
      | mk [x] = Coda x
      | mk [x, y] = PostCoda (x, y)
      | mk [x, y, z] = PostCodaCompl (x, y, z)
      | mk _ = raise BadCoda

    fun toStr ZeroCoda = ""
      | toStr (Coda cons) = Consonant.toStr cons
      | toStr (PostCoda (cons1, cons2)) = Consonant.toStr cons1 ^ Consonant.toStr cons2
      | toStr (PostCodaCompl (cons1, cons2, cons3)) 
        = Consonant.toStr cons1 ^ Consonant.toStr cons2 ^ Consonant.toStr cons3
end

signature SYLL = sig
    datatype stress = Stressed | Unstressed

    datatype t = Syll of Onset.t * Nucleus.t * Coda.t * stress

    val is_stressed : t -> bool
    val stress_syll : t -> t

    val mk : (Consonant.t list * Vowel.t list * Consonant.t list) -> t

    val toStr : t -> string
end

structure Syll : SYLL = struct
    datatype stress = Stressed | Unstressed

    datatype t = Syll of Onset.t * Nucleus.t * Coda.t * stress

    fun is_stressed (Syll (onset, nuc, coda, stress)) 
        = (stress = Stressed)

    fun stress_syll (Syll (onset, nuc, coda, _)) 
        = Syll (onset, nuc, coda, Stressed)

    fun mk (on, nuc, coda) = 
        let val on' = Onset.mk on
            val nuc' = Nucleus.mk nuc
            val coda' = Coda.mk coda
        in Syll (on', nuc', coda', Unstressed) end

    fun toStr (Syll (on, nuc, cod, stress)) =
        let val on' = Onset.toStr on
            val nuc' = Nucleus.toStr nuc
            val cod' = Coda.toStr cod
        in if stress = Stressed
           then "'" ^ on' ^ nuc' ^ cod'
           else on' ^ nuc' ^ cod'
        end
end

signature PWORD = sig
    exception BadPWord

    type t = Syll.t list

    val is_heavy : Syll.t -> bool
    val assign_stress : Syll.t list -> t

    val mk : Syll.t list -> t

    val toStr : t -> string

    val show : t -> unit
end

structure PWord : PWORD = struct
    exception BadPWord

    type t = Syll.t list

    fun is_heavy (Syll.Syll (onset, nuc, coda, _)) 
        = case (nuc, coda) 
           of (Nucleus.Monophthong _, Coda.ZeroCoda) => false
            | _ => true

    fun assign_stress [] = raise BadPWord
      | assign_stress [x] = [Syll.stress_syll x]
      | assign_stress [x, y] = [Syll.stress_syll x, y]
      | assign_stress pword 
        = let val (x :: y :: z :: xs) = rev pword
              val reversed
                  = if is_heavy y
                    then (x :: Syll.stress_syll y :: z :: xs)
                    else (x :: y :: Syll.stress_syll z :: xs)
          in rev reversed end

    fun mk [] = raise BadPWord
      | mk l = assign_stress l

    fun toStr [] = ""
      | toStr [x] = Syll.toStr x
      | toStr (x :: xs) = (Syll.toStr x) ^ "." ^ (toStr xs)

    fun show pword = print (toStr pword ^ "\n")
end
