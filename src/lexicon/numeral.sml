structure Numeral = struct
    val unum_etymon = 
        let val u = let val on = []
                        val nuc = [Seg.u, Seg.u]
                        val coda = []
                    in Syll.mk (on, nuc, coda) end
            val num = let val on = [Seg.n]
                          val nuc = [Seg.u]
                          val coda = [Seg.m]
                      in Syll.mk (on, nuc, coda) end
        in PWord.mk [u, num] end
end
