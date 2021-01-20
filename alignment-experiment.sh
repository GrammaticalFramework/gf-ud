gfud -ud2gf grammars/Extract Eng Utt at <out/aligns_en.conllu >out/gfts-en.tmp 
gfud -ud2gf grammars/Extract Ita Utt at <out/aligns_it.conllu >out/gfts-it.tmp 
paste out/gfts-en.tmp out/gfts-it.tmp | grep -v Backup | sort -u >out/sorted-aligned-gfts.tmp

