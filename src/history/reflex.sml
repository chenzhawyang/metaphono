signature REFLEX = sig
    datatype t = T of PWord.t * string

    val append : string -> t -> t

    val mk : PWord.t -> t
    val fromSylls : Syll.t list -> t

    val apply : Rule.t -> t -> t
    val apply_l : (string * (Rule.t list)) -> t -> t
    val apply_ll : (string * (Rule.t list)) list -> t -> t

    val toStr : t -> string
    val show : t -> unit
end

structure Reflex : REFLEX = struct
    datatype t = T of PWord.t * string

    fun append record reflex = 
        let val T (pword, history) = reflex
            val history' = history ^ record
        in T (pword, history') end

    fun mk pword = 
        let val etymon = ":etymon:\n" ^ PWord.toStr pword
        in T (pword, etymon) end

    fun fromSylls sylls = 
        (mk o PWord.mk) sylls

    fun apply rule reflex = 
        let val T (pword, history) = reflex
            val Rule.T (name, change) = rule
            val pword' = change pword
            val record
                = PWord.toStr pword 
                  ^ " => " 
                  ^ PWord.toStr pword' 
                  ^ " (" ^ name ^ ")"
            val history' = history ^ "\n" ^ record
        in 
            if pword = pword'
            then reflex
            else T (pword', history')
        end

    local
        fun apply_l' [] reflex = reflex
          | apply_l' (x :: xs) reflex =
            let val reflex' = apply x reflex
            in apply_l' xs reflex' end
    in
        fun apply_l (_, []) reflex = reflex
          | apply_l (name, rules) reflex = 
            let val reflex' = append ("\n:era:\n" ^ name ^ "\n:change:") reflex
                val reflex'' = apply_l' rules reflex'
                val T (pword, _) = reflex
                val T (pword', _) = reflex''
            in if pword <> pword'
               then reflex''
               else reflex
            end
    end

    fun apply_ll [] reflex = reflex
      | apply_ll (x :: xs) reflex =
        let val reflex' = apply_l x reflex
        in apply_ll xs reflex' end

    fun toStr (T (pword, history)) = history
    fun show reflex = print (toStr reflex ^ "\n")
end

structure Gloss 
: sig 
    val toStr : Reflex.t list -> string
    val show : Reflex.t list -> unit
end 
= struct
    fun toStr [] = ""
      | toStr (x :: xs) = 
        (Reflex.toStr x ^ "\n\n") ^ toStr xs        

    val show = print o toStr
end
