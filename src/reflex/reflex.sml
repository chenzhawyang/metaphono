signature REFLEX = sig
    datatype t = T of PWord.t * SExp.value

    val mk : PWord.t -> t
    val fromSylls : Syll.t list -> t

    val apply_chg_l : string * Rule.t list -> t -> t
    val apply_chg_ll : (string * Rule.t list) list -> t -> t
end

structure Reflex : REFLEX = struct
    datatype t = T of PWord.t * SExp.value

    fun mk pword =
        let val etymon = SExp.LIST [SExp.STRING (PWord.toStr pword)]
        in T (pword, etymon) end

    val fromSylls = mk o PWord.mk

    structure Aux : sig
        val empty_rec : SExp.value
        val apply : Rule.t -> PWord.t -> PWord.t * SExp.value
        val apply_l : string * Rule.t list -> PWord.t -> PWord.t * SExp.value
    end = struct                             
        val empty_rec = SExp.LIST []

        fun apply rule pword = 
            let val Rule.T (name, change) = rule
                val pword' = change pword
                val record = SExp.LIST
                               [ SExp.STRING (PWord.toStr pword)
                               , SExp.STRING (PWord.toStr pword') 
                               , SExp.STRING name ]
            in 
                if pword = pword'
                then (pword', empty_rec)
                else (pword', record)
            end

            (* fun apply_l (name, rules) pword =  *)
            (*     let fun mk_l (n, r) = SExp.LIST [SExp.STRING n, r] *)
            (*         fun aux (pword, rules, record) =  *)
            (*             case rules  *)
            (*              of [] => (pword, mk_l (name, record)) *)
            (*               | (x :: xs) => let val (pword', record') = apply x pword *)
            (*                              in case record'  *)
            (*                                  of SExp.LIST [] => aux (pword', xs, record) *)
            (*                                   | SExp.LIST l =>  *)
            (*                                     let val (SExp.LIST l, SExp.LIST l') = (record, record') *)
            (*                                         val new_l = SExp.LIST (l @ l') *)
            (*                                     in aux (pword', xs, new_l) *)
            (*                             end  *)
            (*     in aux (pword, rules, empty_rec) end *)
    end

    fun apply_chg_l (name, rules) reflex =
        let val T (pword, history) = reflex
            val (pword', record) = Aux.apply_l (name, rules) pword 
            val SExp.LIST h = history
        in T (pword', history') end
            
    fun apply_chg_ll [] reflex = reflex
      | apply_chg_ll (x :: xs) reflex = 
        let val (name, rules) = x
            val reflex' = apply_chg_l (name, rules) reflex
        in apply_chg_ll xs reflex' end
end
