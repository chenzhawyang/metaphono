structure Lexicon = struct
    val uunum = let val uu = Syll.mk ([], [Seg.u, Seg.u], [])
                    val num = Syll.mk ([Seg.n], [Seg.u], [Seg.m])
                    val etymon = Reflex.fromSylls [uu, num]
                in History.pidal etymon end

    val decem = let val de = Syll.mk ([Seg.d], [Seg.e], [])
                    val cem = Syll.mk ([Seg.k], [Seg.e], [Seg.m])
                    val etymon = Reflex.fromSylls [de, cem]
                in History.pidal etymon end

    val lupum = let val lu = Syll.mk ([Seg.l], [Seg.u], [])
                    val pum = Syll.mk ([Seg.p], [Seg.u], [Seg.m])
                    val etymon = Reflex.fromSylls [lu, pum]
                in History.pidal etymon end

    val veenaatum = let val vee = Syll.mk ([Seg.w], [Seg.e, Seg.e], [])
                        val naa = Syll.mk ([Seg.n], [Seg.a, Seg.a], [])
                        val tum = Syll.mk ([Seg.t], [Seg.u], [Seg.m])
                        val etymon = Reflex.fromSylls [vee, naa, tum]
                    in History.pidal etymon end

    val petram = let val pe = Syll.mk ([Seg.p], [Seg.e], [])
                     val tram = Syll.mk ([Seg.t, Seg.r], [Seg.a], [Seg.m])
                     val etymon = Reflex.fromSylls [pe, tram]
                 in History.pidal etymon end

    val thesaurum = let val the = Syll.mk ([Seg.t], [Seg.e], [])
                        val sau = Syll.mk ([Seg.s], [Seg.a, Seg.u], [])
                        val rum = Syll.mk ([Seg.r], [Seg.u], [Seg.m])
                        val etymon = Reflex.fromSylls [the, sau, rum]
                    in History.pidal etymon end

    val portam = let val por = Syll.mk ([Seg.p], [Seg.seg_o], [Seg.r])
                     val tam = Syll.mk ([Seg.t], [Seg.a], [Seg.m])
                     val etymon = Reflex.fromSylls [por, tam]
                 in History.pidal etymon end

    val caelum = let val cae = Syll.mk ([Seg.k], [Seg.a, Seg.i], [])
                     val lum = Syll.mk ([Seg.l], [Seg.u], [Seg.m])
                     val etymon = Reflex.fromSylls [cae, lum]
                 in History.pidal etymon end    

    val casam = let val ca = Syll.mk ([Seg.k], [Seg.a], [])
                    val sam = Syll.mk ([Seg.s], [Seg.a], [Seg.m])
                    val etymon = Reflex.fromSylls [ca, sam]
                in History.pidal etymon end

    val luucem = let val luu = Syll.mk ([Seg.l], [Seg.u, Seg.u], [])
                     val cem = Syll.mk ([Seg.k], [Seg.e], [Seg.m])
                     val etymon = Reflex.fromSylls [luu, cem]
                 in History.pidal etymon end

    val scholam = let val scho = Syll.mk ([Seg.s, Seg.k], [Seg.seg_o], [])
                      val lam = Syll.mk ([Seg.l], [Seg.a], [Seg.m])
                      val etymon = Reflex.fromSylls [scho, lam]
                  in History.pidal etymon end

    val iocum = let val io = Syll.mk ([Seg.j], [Seg.seg_o], [])
                    val cum = Syll.mk ([Seg.k], [Seg.u], [Seg.m])
                    val etymon = Reflex.fromSylls [io, cum]
                in History.pidal etymon end

    val folia = let val fo = Syll.mk ([Seg.ph], [Seg.seg_o], [])
                    val lia = Syll.mk ([Seg.l], [Seg.i, Seg.a], [])
                    val etymon = Reflex.fromSylls [fo, lia]
                in History.pidal etymon end

    val fiilium = let val fii = Syll.mk ([Seg.ph], [Seg.i, Seg.i], [])
                     val lium = Syll.mk ([Seg.l], [Seg.i, Seg.u], [Seg.m])
                     val etymon = Reflex.fromSylls [fii, lium]
                 in History.pidal etymon end

    val focum = let val fo = Syll.mk ([Seg.ph], [Seg.seg_o], [])
                    val cum = Syll.mk ([Seg.k], [Seg.u], [Seg.m])
                    val etymon = Reflex.fromSylls [fo, cum]
                in History.pidal etymon end

    val socra = let val so = Syll.mk ([Seg.s], [Seg.seg_o], [])
                    val cra = Syll.mk ([Seg.k, Seg.r], [Seg.a], [])
                    val etymon = Reflex.fromSylls [so, cra]
                in History.pidal etymon end

    val ossum = let val os = Syll.mk ([], [Seg.seg_o], [Seg.s])
                    val sum = Syll.mk ([Seg.s], [Seg.u], [Seg.m])
                    val etymon = Reflex.fromSylls [os, sum]
                in History.pidal etymon end

    val africum = let val a = Syll.mk ([], [Seg.a], [])
                      val fri = Syll.mk ([Seg.ph, Seg.r], [Seg.i], [])
                      val cum = Syll.mk ([Seg.k], [Seg.u], [Seg.m])
                      val etymon = Reflex.fromSylls [a, fri, cum]
                  in History.pidal etymon end

    val fiicum = let val fii = Syll.mk ([Seg.ph], [Seg.i, Seg.i], [])
                     val cum = Syll.mk ([Seg.k], [Seg.u], [Seg.m])
                     val etymon = Reflex.fromSylls [fii, cum]
                 in History.pidal etymon end

    val iovis = let val io = Syll.mk ([Seg.j], [Seg.seg_o], [])
                    val vis = Syll.mk ([Seg.w], [Seg.i], [Seg.s])
                    val etymon = Reflex.fromSylls [io, vis]
                in History.pidal etymon end

    val annum = let val an = Syll.mk ([], [Seg.a], [Seg.n])
                    val num = Syll.mk ([Seg.n], [Seg.u], [Seg.m])
                    val etymon = Reflex.fromSylls [an, num]
                in History.pidal etymon end

    val entries = 
        [ uunum
        , decem
        , lupum
        , veenaatum 
        , petram 
        , thesaurum 
        , portam 
        , caelum 
        , casam 
        , luucem 
        , scholam 
        , iocum 
        , folia 
        , fiilium 
        , focum 
        , socra 
        , ossum
        , africum 
        , fiicum 
        , iovis 
        , annum ]
end
