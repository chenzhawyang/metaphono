signature EPOCH = sig
    val name : string
    val history : Rule.t list
    val epoch : string * Rule.t list
end
