signature REFLEX = sig
    type record = string * string * string
    datatype t = T of PWord.t * record list

    val mk : PWord.t -> t
    val fromSylls : Syll.t list -> t

    val apply : Rule.t -> t -> t
    val apply_l : Rule.t list -> t -> t
    val apply_ll : Rule.t list list -> t -> t
end
                       
structure Reflex : REFLEX = struct
    type record = string * string * string
    datatype t = T of PWord.t * record list
    
    fun mk pword = T (pword, []) 

    val fromSylls = mk o PWord.mk

    fun apply rule reflex = 
        let val T (pword, history) = reflex
            val Rule.T (name, change) = rule
            val pword' = change pword
            val record = ((PWord.toStr pword)
                         , (PWord.toStr pword')
                         , name)
            val history' = history @ [record]
        in 
            if pword = pword'
            then reflex
            else T (pword', history')
        end

    (* foldl? *)
    fun apply_l [] reflex = reflex
      | apply_l (x :: xs) reflex = let val reflex' = apply x reflex
                                  in apply_l xs reflex' end

    fun apply_ll [] reflex = reflex
      | apply_ll (x :: xs) reflex = let val reflex' = apply_l x reflex
                                   in apply_ll xs reflex' end
end
