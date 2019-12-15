# gf-ud
Conversions between GF and dependency trees, in particular UD (Universal Dependencies) 
but designed to be completely generic as for annotation scheme. 
This repository replaces the old gf-contrib/ud2gf code.

The conversions are based on algorithms presented in the papers

- P Kolachina and A Ranta, From Abstract Syntax to Universal Dependencies, 
  LiLT (Linguistic Issues in Language Technology) 13, 2016 ("gf2ud")
  http://csli-lilt.stanford.edu/ojs/index.php/LiLT/article/view/71/73 
  
- A Ranta and P Kolachina, From Universal Dependencies to Abstract Syntax,
  Proceedings of the NoDaLiDa 2017 Workshop on Universal Dependencies, 2017 ("ud2gf")
  https://www.aclweb.org/anthology/W17-0414.pdf
  
as well as some earlier code in gf-contrib/ud2gf and in gf-core/src/, mostly by 
Aarne Ranta and Prasanth Kolachina. Differences from those sources may be documented 
later. 

To get started, you can test the following:

- cd grammars
- ln -s gf-summerschool-2018  # a sister Git repository
- make mini
- cd ..
- make compile
- cat test.conllu | ./gfud -ud2gf grammars/MiniLang Eng Utt at
- cat test.gftrees | ./gfud -gf2ud grammars/MiniLang Eng Utt ud
- echo "the black cat sees us" | ./gfud -string2gf2ud grammars/MiniLang Eng Utt 
  
The largest changes are perhaps in the format of dependency configurations.
One goal was to merge the gf2ud and ud2gf annotations. See

- grammars/MiniLang.labels
- grammars/MiniLangEng.labels
  
for examples (to be documented properly later).

Other grammars to try are

- grammars/ShallowParse  # wide-coverage, with WordNet words but without sense distinctions, uses sister repo gf-wordnet/
- grammars/Term  # a grammar of arithmetic expression, completely different tagset

With ShallowParse, you can try out the included sample from https://github.com/UniversalDependencies/UD_English-PUD/blob/master/en_pud-ud-test.conllu

- cat upto12eng.conllu | ./gfud -ud2gf grammars/ShallowParse Eng Text at stat

But the results are still meager, and require additions in ShallowParse.labels and maybe in the algorithm.






  
  
