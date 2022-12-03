use "history.sml";

(* numerals *)

local val sep = let val on = Onset seg_s
                    val nuc = Monophthong seg_e
                    val cod = Codetta seg_p
                in Syllable (on, nuc, cod, Stressed) end
      val tem = let val on = Onset seg_t
                    val nuc = Monophthong seg_e
                    val cod = Codetta seg_m
                in Syllable (on, nuc, cod, Unstressed) end
      val pword = [sep, tem]
in val septem_etymon = mkEtymon pword end

val septem_refl = MenendezPidal historia septem_etymon

local val de = let val on = Onset seg_d
                   val nuc = Monophthong seg_e
                   val cod = ZeroCoda
               in Syllable (on, nuc, cod, Stressed) end
      val cem = let val on = Onset seg_k
                    val nuc = Monophthong seg_e
                    val cod = Codetta seg_m
                in Syllable (on, nuc, cod, Unstressed) end
      val pword = [de, cem]
in val decem_etymon = mkEtymon pword end

val decem_refl = MenendezPidal historia decem_etymon

(* misc. nouns *)

local val lu =
          let val on = Onset seg_l
	      val nuc = Monophthong seg_u
	      val cod = ZeroCoda
          in Syllable (on, nuc, cod, Stressed) end
      val pum =
          let val on = Onset seg_p
	      val nuc = Monophthong seg_u
	      val cod = Codetta seg_m
          in Syllable (on, nuc, cod, Unstressed) end
      val pword = [lu, pum]
in val lupum_etymon = mkEtymon pword end

val lupum_refl = MenendezPidal historia lupum_etymon

local val vee =
          let val on = Onset seg_w
              val nuc = LongVowel seg_e
              val cod = ZeroCoda
          in Syllable (on, nuc, cod, Unstressed) end
      val naa =
          let val on = Onset seg_n
              val nuc = LongVowel seg_a
              val cod = ZeroCoda
          in Syllable (on, nuc, cod, Stressed) end
      val tum =
          let val on = Onset seg_t
              val nuc = Monophthong seg_u
              val cod = Codetta seg_m
          in Syllable (on, nuc, cod, Unstressed) end
      val pword =  [vee, naa, tum]
in val veenaatum_etymon = mkEtymon pword end

local val veenaatum_prRom = applyLangShift prRom_lang_shift veenaatum_etymon
      val veenaatum_WRom = applyLangShift WRom_lang_shift veenaatum_prRom
      val veenaatum_OSp = applyLangShift OSp_lang_shift veenaatum_WRom
      val veenaatum_Es = applyLangShift Es_lang_shift veenaatum_OSp
in val veenaatum_refl = veenaatum_Es end

local val pe = let val on = Onset seg_p
                   val nuc = Monophthong seg_e
                   val cod = ZeroCoda
               in Syllable (on, nuc, cod, Stressed) end
      val tram = let val on = OnsetM (seg_t, seg_rr)
                     val nuc = Monophthong seg_a
                     val cod = Codetta seg_m
                 in Syllable (on, nuc, cod, Unstressed) end
      val pword = [pe, tram]
in val petram_etymon = mkEtymon pword end

val petram_refl = MenendezPidal historia petram_etymon

local val the = let val on = Onset seg_t
                    val nuc = Monophthong seg_e
                    val cod = ZeroCoda
                in Syllable (on, nuc, cod, Unstressed) end
      val sau = let val on = Onset seg_s
                    val nuc = Diphthong (seg_a, seg_u)
                    val cod = ZeroCoda
                in Syllable (on, nuc, cod, Stressed) end
      val rum = let val on = Onset seg_rr
                    val nuc = Monophthong seg_u
                    val cod = Codetta seg_m
                in Syllable (on, nuc, cod, Unstressed) end
      val pword = [the, sau, rum]
in val thesaurum_etymon = mkEtymon pword end

val thesaurum_refl = MenendezPidal historia thesaurum_etymon

local val por = let val on = Onset seg_p
                    val nuc = Monophthong seg_o
                    val cod = Codetta seg_rr
                in Syllable (on, nuc, cod, Stressed) end
      val tam = let val on = Onset seg_t
                    val nuc = Monophthong seg_a
                    val cod = Codetta seg_m
                in Syllable (on, nuc, cod, Unstressed) end
      val pword = [por, tam]
in val portam_etymon = mkEtymon pword end

local val portam_prRom = applyLangShift prRom_lang_shift portam_etymon
      val portam_WRom = applyLangShift WRom_lang_shift portam_prRom
      val portam_OSp = applyLangShift OSp_lang_shift portam_WRom
in val portam_refl = portam_OSp end

local val gen = let val on = Onset seg_dg
                    val nuc = Monophthong seg_e
                    val cod = Codetta seg_n
                in Syllable (on, nuc, cod, Stressed) end
      val te = let val on = Onset seg_t
                    val nuc = Monophthong seg_e
                    val cod = ZeroCoda
                in Syllable (on, nuc, cod, Unstressed) end
      val pword = [gen, te]
in val gente_Rom = mkEtymon pword end

local val gente_Es = applyLangShift Es_lang_shift gente_Rom
in val gente_refl = gente_Es end

local val cae = let val on = Onset seg_k
                    val nuc = let val v1 = seg_a
                                  val v2 = seg_e
                              in Diphthong (v1, v2) end
                    val cod = ZeroCoda
                in Syllable (on, nuc, cod, Stressed) end
      val lum = let val on = Onset seg_l
                    val nuc = Monophthong seg_u
                    val cod = Codetta seg_m
                in Syllable (on, nuc, cod, Unstressed) end
      val pword = [cae, lum]
in val caelum_etymon = mkEtymon pword end

val caelum_refl = MenendezPidal historia caelum_etymon

local val ra = let val on = Onset seg_rr
                   val nuc = Monophthong seg_a
                   val cod = ZeroCoda
               in Syllable (on, nuc, cod, Stressed) end
      val tion = let val on = Onset seg_t
                     val nuc = let val v1 = seg_i
                                     val v2 = seg_o
                                 in Diphthong (v1, v2) end
                     val cod = Codetta seg_n
                 in Syllable (on, nuc, cod, Unstressed) end
      val pword = [ra, tion]
in val ration_Rom = mkEtymon pword end

val ration_refl = MenendezPidal historia ration_Rom

local val ca = let val on = Onset seg_k
                   val nuc = Monophthong seg_a
                   val cod = ZeroCoda
               in Syllable (on, nuc, cod, Stressed) end
      val sam = let val on = Onset seg_s
                    val nuc = Monophthong seg_a
                    val cod = Codetta seg_m
                in Syllable (on, nuc, cod, Unstressed) end
      val pword = [ca, sam]
in val casam_etymon = mkEtymon pword end

val casam_refl = MenendezPidal historia casam_etymon

local val luu = let val on = Onset seg_l
                   val nuc = LongVowel seg_u
                   val cod = ZeroCoda
               in Syllable (on, nuc, cod, Stressed) end
      val cem = let val on = Onset seg_k
                    val nuc = Monophthong seg_e
                    val cod = Codetta seg_m
                in Syllable (on, nuc, cod, Unstressed) end
      val pword = [luu, cem]
in val luucem_etymon = mkEtymon pword end

val luucem_refl = MenendezPidal historia luucem_etymon 

local val scho = let val on = let val cons1 = seg_s
                                  val cons2 = seg_k
                              in POnset (cons1, cons2) end
                     val nuc = Monophthong seg_o
                     val cod = ZeroCoda
                 in Syllable (on, nuc, cod, Stressed) end
      val lam = let val on = Onset seg_l
                    val nuc = Monophthong seg_a
                    val cod = Codetta seg_m
                in Syllable (on, nuc, cod, Unstressed) end
      val pword = [scho, lam]
in val scholam_etymon = mkEtymon pword end

val scholam_refl = MenendezPidal historia scholam_etymon

local val io = let val on = Onset seg_j
                   val nuc = Monophthong seg_o
                   val cod = ZeroCoda
               in Syllable (on, nuc, cod, Stressed) end
      val cum = let val on = Onset seg_k
                    val nuc = Monophthong seg_u
                    val cod = Codetta seg_m
                in Syllable (on, nuc, cod, Unstressed) end
      val pword = [io, cum]
in val iocum_etymon = mkEtymon pword end

val iocum_refl = MenendezPidal historia iocum_etymon

local val fo = let val on = Onset seg_ph
                   val nuc = Monophthong seg_o
                   val cod = ZeroCoda
               in Syllable (on, nuc, cod, Stressed) end
      val lia = let val on = Onset seg_l
                    val nuc = let val v1 = seg_i
                                  val v2 = seg_a
                              in Diphthong (v1, v2) end
                    val cod = ZeroCoda
                in Syllable (on, nuc, cod, Unstressed) end
      val pword = [fo, lia]
in val folia_etymon = mkEtymon pword end

val folia_refl = MenendezPidal historia folia_etymon

local val fi = let val on = Onset seg_ph
                   val nuc = LongVowel seg_i
                   val cod = ZeroCoda
               in Syllable (on, nuc, cod, Stressed) end
      val lium = let val on = Onset seg_l
                     val nuc = let val v1 = seg_i
                                   val v2 = seg_u
                               in Diphthong (v1, v2) end
                     val cod = Codetta seg_m
                 in Syllable (on, nuc, cod, Unstressed) end
      val pword = [fi, lium]
in val filium_etymon = mkEtymon pword end

val filium_refl = MenendezPidal historia filium_etymon

local val fo = let val on = Onset seg_ph
                   val nuc = Monophthong seg_o
                   val cod = ZeroCoda
               in Syllable (on, nuc, cod, Stressed) end
      val cum = let val on = Onset seg_k
                    val nuc = Monophthong seg_u
                    val cod = Codetta seg_m
                in Syllable (on, nuc, cod, Unstressed) end
      val pword = [fo, cum]
in val focum_etymon = mkEtymon pword end

val focum_refl = MenendezPidal historia focum_etymon

local val so = let val on = Onset seg_s
                   val nuc = Monophthong seg_o
                   val cod = ZeroCoda
               in Syllable (on, nuc, cod, Stressed) end
      val cru = let val on = let val cons1 = seg_k
                                 val cons2 = seg_rr
                             in OnsetM (cons1, cons2) end
                    val nuc = Monophthong seg_u
                    val cod = ZeroCoda
                in Syllable (on, nuc, cod, Unstressed) end
      val pword = [so, cru]
in val socru_etymon = mkEtymon pword end

val socru_refl = MenendezPidal historia socru_etymon

local val os = let val on = ZeroOnset
                   val nuc = Monophthong seg_o
                   val cod = Codetta seg_s
               in Syllable (on, nuc, cod, Stressed) end
      val sum = let val on = Onset seg_s
                    val nuc = Monophthong seg_u
                    val cod = Codetta seg_m
                in Syllable (on, nuc, cod, Unstressed) end
      val pword = [os, sum]
in val ossum_etymon = mkEtymon pword end

val ossum_refl = MenendezPidal historia ossum_etymon

local val a = let val on = ZeroOnset
                  val nuc = Monophthong seg_a
                  val cod = ZeroCoda
              in Syllable (on, nuc, cod, Stressed) end
      val fri = let val on = let val cons1 = seg_ph
                                 val cons2 = seg_rr
                             in OnsetM (cons1, cons2) end
                    val nuc = Monophthong seg_i
                    val cod = ZeroCoda
                in Syllable (on, nuc, cod, Unstressed) end
      val cu = let val on = Onset seg_k
                   val nuc = Monophthong seg_u
                   val cod = ZeroCoda
               in Syllable (on, nuc, cod, Unstressed) end
      val pword = [a, fri, cu]
in val africu_etymon = mkEtymon pword end

val africu_refl = MenendezPidal historia africu_etymon
