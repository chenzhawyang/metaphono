signature REFLEX = sig
    type record
    type reflex
end

structure Reflex : REFLEX = struct
    open PWord

    datatype record = Record

    datatype reflex = Reflex of pWord * record list
end
