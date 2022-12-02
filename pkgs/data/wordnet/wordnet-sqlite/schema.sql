create table word(wordno, writtenform);
create table lexrel(wordno1, synsetno1, wordno2, synsetno2, reltypeno);
create table semrel(synsetno1, synsetno2, reltypeno);
create table reltype(reltypeno, speech, reltypename);
create table synset(synsetno, definition, lexfilenum);
create table sense(wordno, synsetno, tagcnt);
create table lexname(lexfilenum, lexname, definition)
