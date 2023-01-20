

# Introduction

Spanish is among the most well-studied languages. With the advancement of Romance historical linguistics, we almost know the full picture of its historical phonological changes. For example, here is the full development of the Latin etymon FĪLIUM to its Spanish reflex *hijo*:

-   'ɸiː.li̯um => 'ɸiː.li̯u (Elision of Word-Final [m])
-   'ɸiː.li̯u => 'ɸi.li̯ʊ (Loss of Vowel Quantity)
-   'ɸi.li̯ʊ => 'ɸi.li̯o (Great Merger)
-   'ɸi.li̯o => 'ɸi.ʎo (Palatalization of [l])
-   'ɸi.ʎo => 'ɸi.ʒo (Spirantization of [ʎ])
-   'ɸi.ʒo => 'hi.ʒo (Debuccalization of [ɸ])
-   'hi.ʒo => 'i.ʒo (Deaspiration of [h])
-   'i.ʒo => 'i.ʃo (Devoicing of Sibilants)
-   'i.ʃo => 'i.xo (Retraction of [ʃ])

There are many, many other Spanish words than can be derived from Latin through regular sound changes.   
Regular sound changes can be automated in a high-level programming language. In this project, I had chosen ML to implement a couple dozen sound changes from Latin to Spanish to demonstrate how to automate a good chunk of regular sound changes that a linguist would encounter fairly often at work.   


## ML

The ML dialect I chose is Standard ML '97. The program is compiled by the SML/NJ compiler. ML is a relatively small language with clear (in fact, formally defined) semantics. To have a taste of what ML looks like, take a look at the following examples.


### Idiomatic definition of natural numbers

    signature NAT = sig
        datatype t = Zero
                   | Succ of t
    
        val pred : t -> t
        val plus : t -> t -> t
    end


### Defining a map function

    structure MyMap : sig
        val map : ('a -> 'b) -> 'a list -> 'b list
    end = struct
        fun map _ [] = []
          | map f (x :: xs) = f x :: map f xs
    end

Those who want to know more about ML can read [4] written by MacQueen et al. It will be apparent in the next section that any high-level programming language will be able to do the tasks here; but a language with an ISWIM-like syntax is just particularly convenient to serve as a computational vernacular. Readers may feel free to implement the program in languages they desire, e.g. Haskell (which is by and large trivial) or some dialect of Lisp.


## Spanish Historical Phonology

Romance historical linguistics is a fruitful subfield of historical linguistics. In our particular interest, many excellent monographs on Spanish historical phonology were published in the past few decades [1, 2, 3], enabling non-experts like me to have a good grasp of the subject. Almost all sound changes implemented here were found in these sources. Curious readers may find these works quite amusing.


# Gist of the Program

Computing is not really the theme of this project. And the programmatic techniques we used here elementary - the only data structure that I used was the list, and it hardly used any advanced operations mentioned in [5], a great reference to the usage of lists.   
Back to our program. In a bird's-eye view, this program rewrites a list to another list, the element in those lists being trees (syllables):

    val change : Syll.t list -> Syll.t list

Syllables look something like

    signature Syll = sig
      datatype t = T of Onset.t * Nucleus.t * Coda.t * Stress.t
    end

Here is an early sound change in this history of Latin-Romance:

    structure LossOfQuant : sig
        val vocalism : Vowel.t -> Vowel.t
        val nuxism : Nucleus.t -> Nucleus.t
        val syllabism : Syll.t -> Syll.t
        val change : Syll.t list -> Syll.t list
    end = struct
        fun vocalism (Vowel (High, cent)) = Vowel (HighMid, cent)
          | vocalism (Vowel (Mid, cent)) = Vowel (LowMid, cent)
          | vocalism v = v
    
        fun nuxism (Monophthong v) = Monophthong (vocalism v)
          | nuxism (Long v) = Monophthong v
          | nuxism nuc = nuc
    
        fun syllabism Syll (onset, nuc, coda, stress) =
            Syll (onset, nuxism nuc, coda, stress)
    
        val change = map syllabism
    end

in which the Latin short vowels [i, u, e, o] were lowered one degree to [ɪ, ʊ, ɛ, ɔ]. Those who are familiar with Latin-Romance probably know that [ɪ, ʊ] were merged to [e, o] in most Romance languages, and [ɛ, ɔ] diphthongize in Castilian (DECEM -> *diez*, OSSUM -> *hueso*).   
By now the readers may find the programming part of this project disenchanted, which is the goal of this section.   
This program has a simple architecture: there is a  *static* part, in which we define ways of representing vowels, consonants, syllables, and phonological words in Spanish and its Latin-Romance predecessors; there is a *dynamic* part, in which we define various utilities to create sound changes and apply them to a phonological word.


# Representing Segments

Here we define the vowels and consonants we need for doing Spanish historical phonology.


## Vowel

We would need the following vowels to cover the history of Spanish:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">&#xa0;</th>
<th scope="col" class="org-left">Front</th>
<th scope="col" class="org-left">Cent.</th>
<th scope="col" class="org-left">Back</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">High</td>
<td class="org-left">i</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">u</td>
</tr>


<tr>
<td class="org-left">High-Mid</td>
<td class="org-left">ɪ</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">ʊ</td>
</tr>


<tr>
<td class="org-left">Mid</td>
<td class="org-left">e</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">o</td>
</tr>


<tr>
<td class="org-left">Low-Mid</td>
<td class="org-left">ɛ</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">ɔ</td>
</tr>


<tr>
<td class="org-left">Low</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">a</td>
<td class="org-left">&#xa0;</td>
</tr>
</tbody>
</table>

The table is trivially translated to ML:

    structure Height = struct
        datatype t = L              (* Low *)
                   | LM             (* Low-Mid *)
                   | M              (* Mid *)
                   | HM             (* High-Mid *)
                   | H              (* High *)
    end
    
    structure Cent = struct
        datatype t = Fr              (* Front *)
                   | Ct              (* Central *)
                   | Bk              (* Back *)
    end
    
    structure Vowel = struct
        datatype t = T of Height.t * Cent.t
    end


## Consonant

And these are the consonants we would need:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">&#xa0;</th>
<th scope="col" class="org-left">Bilabial</th>
<th scope="col" class="org-left">Labiodental</th>
<th scope="col" class="org-left">Dental</th>
<th scope="col" class="org-left">Alveolar</th>
<th scope="col" class="org-left">Palatal</th>
<th scope="col" class="org-left">Velar</th>
<th scope="col" class="org-left">Labiovelar</th>
<th scope="col" class="org-left">Glottal</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">Nasal</td>
<td class="org-left">m</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">n</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">ɲ</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">Stop</td>
<td class="org-left">p b</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">t d</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">k ɡ</td>
<td class="org-left">kʷ ɡʷ</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">Non-Sibilant</td>
<td class="org-left">ɸ β</td>
<td class="org-left">f</td>
<td class="org-left">θ ð</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">ʝ</td>
<td class="org-left">x ɣ</td>
<td class="org-left">xʷ ɣʷ</td>
<td class="org-left">h</td>
</tr>


<tr>
<td class="org-left">Sibilant</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">s̪ z̪</td>
<td class="org-left">s̺ z̺</td>
<td class="org-left">ʃ ʒ</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">Affricate</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">ts dz</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">tʃ dʒ</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">Approximant</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">j</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">w</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">Tap</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">ɾ</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">Trill</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">r</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">Lateral</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">l</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">ʎ</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>
</tbody>
</table>

Their ML representation:

    structure Voice = struct
        datatype t = Voiced
                   | Voiceless
    end
    
    structure Place = struct
        datatype t = Bilabial | Labiodental
                   | Dental
                   | Alveolar
                   | Palatal
                   | Velar | Labiovelar
                   | Glottal
    end
    
    structure Manner = struct
        datatype t = Nasal
                   | Stop
                   | NonSibil
                   | Sibilant
                   | Affricate
                   | Approximant
                   | Tap
                   | Trill
                   | Lateral
    end
    
    structure Consonant = struct
        datatype t = T of Voice.t * Place.t * Manner.t
    end


# Representing Syllables

    structure Onset = struct
        datatype t = Zero
                   | Singl of Consonant.t (* Singleton *)
                   | Comp of Consonant.t * Consonant.t (* Complex *)
                   | Preinit of Consonant.t * Consonant.t (* /sC/ *)
                   | PreinitCompl of Consonant.t * Consonant.t * Consonant.t (* /sCr/ *)
    end
    
    structure Nucleus = struct
        datatype t = Monophthong of Vowel.t
                   | Diphthong of Vowel.t * Vowel.t
                   | Long of Vowel.t
    end
    
    structure Coda = struct
        datatype t = Zero
                   | Coda of Consonant.t
                   | PostCoda of Consonant.t * Consonant.t (* Cs *)
                   | PostCodaCompl of Consonant.t * Consonant.t * Consonant.t (* rCs *)
    end
    
    structure Stress = struct
        datatype t = S              (* Stressed *)
                   | U              (* Unstressed *)
    end
    
    structure Syll = struct
        datatype t = T of Onset.t * Nucleus.t * Coda.t * Stress.t
    end


# References

-   [1] Paul Lloyd. *From Latin to Spanish*.
-   [2] Ralph Penny. *A History of the Spanish Language*.
-   [3] José Ignacio Hualde. *Sounds of Spanish*.
-   [4] David MacQueen et al. *The History of Standard ML*. <https://doi.org/10.1145/3386336>
-   [5] Richard Bird. *An Introduction to the Theory of Lists*. <https://www.cs.ox.ac.uk/files/3378/PRG56.pdf>

