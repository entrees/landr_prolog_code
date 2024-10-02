% Example queries:
% | ?- sentence([Tree],[],[w('PRO','He'),w('VBP',';~I','smiles'),w('CONJ','and'),w('VBP',';~I','smiles'),w('PUNC','.')],[]).
% | ?- tphrase_set_string([w('PRO','He'),w('VBP',';~I','smiles'),w('PUNC','.')]), tphrase(sentence([Tree],[])).

:- import member/2 from basics.
:- import gensym/2 from gensym.

:- auto_table.

% Sentence

sentence([node('IP-MAT',IL)|L],L) -->
  clause_top_layer(statement_order,[],IL,IL1),
  punc(final,IL1,[]).
sentence([node('IP-IMP',IL)|L],L) -->
  clause_top_layer(imperative_clause,[],IL,IL1),
  punc(final,IL1,[]).
sentence([node('CP-QUE-MAT',[node('IP-SUB',IL),PU])|L],L) -->
  clause_top_layer(matrix_interrogative,[],IL,[]),
  punc(final_question,[PU],[]).
sentence([node('IP-MAT',IL)|L],L) -->
  clause_top_layer(statement_order,[],IL,IL2),
  punc(non_final,IL2,[node('CP-QUE-TAG',[node('IP-SUB',TL)])|IL1]),
  clause_top_layer(tag_question,[],TL,[]),
  punc(final_question,IL1,[]).
sentence([node('IP-MAT',[node('ILYR',[node('ILYR',IL1),node('CONJP',[CONJ,node('ILYR',[node('ADVP-CLR',[node('ADV',[node('so',[])])])|IL2])])]),PU])|L],L) -->
  clause_top_layer(statement_order,[],IL1,[]),
  conj(CONJ),
  [w('ADV','so')],
  clause_top_layer(tag_question,[],IL2,[]),
  punc(final,[PU],[]).

% Fragment

fragment([node('FRAG',FL)|L],L) -->
  fragment_layer(FL,FL1),
  punc(final,FL1,[]).

fragment_layer(L,L0) -->
  noun_phrase('',_,L,L0).
fragment_layer(L,L0) -->
  {
    member(Type,[catenative,non_interrogative,interrogative,relative])
  },
  adjective_phrase('',Type,L,L0).
fragment_layer(L,L0) -->
  initial_adverbial(L,L0).
fragment_layer(L,L0) -->
  adverbial(L,L0).
fragment_layer(L,L0) -->
  ip_to_inf('',[],L,L0).
fragment_layer([node('IP-PPL',IL)|L],L) -->
  {
    member(Infl,[hag_participle,en_participle,ing_participle])
  },
  ip_ppl_adverbial_layer(filled_sbj,Infl,IL,[]).
fragment_layer([node('IP-PPL3',IL)|L],L) -->
  {
    member(Infl,[hag_participle,en_participle,ing_participle])
  },
  ip_ppl_adverbial_layer(unfilled_sbj,Infl,IL,[]).

% utterance

utterance(Ext,[node(Label,UL)|L],L) -->
  {
    atom_concat('utterance',Ext,Label)
  },
  utterance_collect(UL,[]).

utterance_collect(L,L0) -->
  sentence(L,L0).
utterance_collect(L,L0) -->
  fragment(L,L0).
utterance_collect(L,L0) -->
  sentence(L,L1),
  utterance_collect(L1,L0).
utterance_collect(L,L0) -->
  fragment(L,L1),
  utterance_collect(L1,L0).

% Nouns

noun([node('NS',[node(Word,[])])|L],L) -->
  [w('NS',Word)].
noun([node('N',[node(Word,[])])|L],L) -->
  [w('N',Word)].
noun([node('NPRS',[node(Word,[])])|L],L) -->
  [w('NPRS',Word)].
noun([node('NPR',[node(Word,[])])|L],L) -->
  [w('NPR',Word)].

noun_head([node('Q;_nphd_',[node(Word,[])])|L],L) -->
  [w('Q;_nphd_',Word)].
noun_head([node('D;_nphd_',[node(Word,[])])|L],L) -->
  [w('D;_nphd_',Word)].

noun_head_full(non_privileged,[node('PNX',[node(Word,[])])|L],L) -->
  [w('PNX',Word)].
noun_head_full(Type,[node('PRO',[node(Word,[])])|L],L) -->
  {
    member(Type,[non_privileged,non_interrogative])
  },
  [w('PRO',Word)].
noun_head_full(Type,[node('NP-GENV',[node('PRO;_ppge_',[node(Word,[])])])|L],L) -->
  {
    member(Type,[non_privileged,non_interrogative])
  },
  [w('PRO;_ppge_',Word)].
noun_head_full(Type,[node('WPRO',[node(Word,[])])|L],L) -->
  {
    member(Type,[non_privileged,interrogative])
  },
  [w('WPRO',Word)].
noun_head_full(relative,[node('RPRO',[node(Word,[])])|L],L) -->
  [w('RPRO',Word)].

% Determiners

det(Type,[node('Q',[node(Word,[])])|L],L) -->
  {
    member(Type,[non_privileged,non_interrogative])
  },
  [w('Q',Word)].
det(Type,[node('D',[node(Word,[])])|L],L) -->
  {
    member(Type,[non_privileged,non_interrogative])
  },
  [w('D',Word)].
det(Type,[node('WD',[node(Word,[])])|L],L) -->
  {
    member(Type,[non_privileged,interrogative])
  },
  [w('WD',Word)].
det(relative,[node('RD',[node(Word,[])])|L],L) -->
  [w('RD',Word)].

% Genitive words

genm([node('GENM',[node(Word,[])])|L],L) -->
  [w('GENM',Word)].

pronoun_genm(Type,[node('PRO;_genm_',[node(Word,[])])|L],L) -->
  {
    member(Type,[non_privileged,non_interrogative])
  },
  [w('PRO;_genm_',Word)].
pronoun_genm(Type,[node('WPRO;_genm_',[node(Word,[])])|L],L) -->
  {
    member(Type,[non_privileged,interrogative])
  },
  [w('WPRO;_genm_',Word)].
pronoun_genm(relative,[node('RPRO;_genm_',[node(Word,[])])|L],L) -->
  [w('RPRO;_genm_',Word)].

% Clause subject

subject(there_sbj,[node('EX',[node(Word,[])])|L],L) -->
  [w('EX',Word)].
subject(cleft_sbj,[node('NP-SBJ',[node('PRO;_cleft_',[node(Word,[])])])|L],L) -->
  [w('PRO;_cleft_',Word)].
subject(expletive_sbj,[node('NP-SBJ',[node('PRO;_expletive_',[node(Word,[])])])|L],L) -->
  [w('PRO;_expletive_',Word)].
subject(provisional_sbj,[node('NP-SBJ',[node('PRO;_provisional_',[node(Word,[])])])|L],L) -->
  [w('PRO;_provisional_',Word)].
subject(filled_sbj,L,L0) -->
  noun_phrase('-SBJ',non_interrogative,L,L0).
subject(derived_sbj,L,L0) -->
  noun_phrase('-SBJ',non_interrogative,L,L0).

% Adverbs

adv(non_interrogative,[node('ADV',[node(Word,[])])|L],L) -->
  [w('ADV',Word)].
adv(non_interrogative,[node('ADVR',[node(Word,[])])|L],L) -->
  [w('ADVR',Word)].
adv(non_interrogative,[node('ADVS',[node(Word,[])])|L],L) -->
  [w('ADVS',Word)].
adv(interrogative,[node('WADV',[node(Word,[])])|L],L) -->
  [w('WADV',Word)].
adv(relative,[node('RADV',[node(Word,[])])|L],L) -->
  [w('RADV',Word)].
adv(particle,[node('RP',[node(Word,[])])|L],L) -->
  [w('RP',Word)].

% Adjectives

adj(non_interrogative,[node('ADJ',[node(Word,[])])|L],L) -->
  [w('ADJ',Word)].
adj(non_interrogative,[node('ADJR',[node(Word,[])])|L],L) -->
  [w('ADJR',Word)].
adj(non_interrogative,[node('ADJS',[node(Word,[])])|L],L) -->
  [w('ADJS',Word)].
adj(catenative,[node('ADJ;_cat_',[node(Word,[])])|L],L) -->
  [w('ADJ;_cat_',Word)].

% Verb word

verb(Infl,Code,[node(TagCode,[node(Word,[])])|L],L) -->
  [w(Tag,Code,Word)],
  {
    verb_tag(Infl,TagList),
    member(Tag,TagList),
    sub_atom(Tag,0,1,_,C),
    verb_code(C,Infl,Code),
    atom_concat(Tag,Code,TagCode)
  }.

% Verb tags

verb_tag(finite,['VBP','VBD','DOP','DOD','HVP','HVD','BEP','BED']).
verb_tag(imperative,['VB','DO','HV','BE']).
verb_tag(infinitive,['VB','DO','HV','BE']).
verb_tag(do_supported_infinitive,['VB','DO','HV']).
verb_tag(ing_participle,['VAG','DAG','HAG','BAG']).
verb_tag(en_participle,['VVN','DON','HVN','BEN']).

% Verb codes

verb_code('V',_,Code) :-
  member(Code,[
     ';~La',';~Ln',
     ';~I',';~Ip',';~Ipr',';~In/pr',';~It',
     ';~Tn',';~Tn.p',';~Tn.pr',
     ';~Tf',';~Tw',';~Tr',
     ';~Tt',';~Tnt',';~Tni',';~Tg',';~Tng',';~Tsg',
     ';~Dn.n',';~Dn.f',';~Dn.w',';~Dn.r',';~Dn.t',';~Dn.*',
     ';~Dpr.n',';~Dpr.f',';~Dpr.r',
     ';~Cn.a',';~Cn.n',';~Cn.n/a',';~Cn.pr',
     ';~Cn.t',';~Cn.i',';~Cn.g',
     ';~V_as_though/as_if/like',
     ';~VP24A',';~VP24C'
    ]).
verb_code('V',Infl,Code) :-
  member(Infl,[finite,infinitive,do_supported_infinitive]),
  member(Code,[';~cat_Vt',';~cat_Vi',';~cat_Vg',';~cat_Vg_passive_',
               ';~cat_Ve_passive_',';~ex_V',';~ex_Vpr',';~ex_cat_Vt']).
verb_code('V',Infl,Code) :-
  member(Infl,[imperative,ing_participle,en_participle]),
  member(Code,[';~cat_Vt',';~cat_Vi',';~cat_Vg',';~cat_Vg_passive_',';~cat_Ve_passive_']).
verb_code('D',_,Code) :-
  member(Code,[';~Tn']).
verb_code('H',_,Code) :-
  member(Code,[';~Tn',';~VP24B',';~VP24C']).
verb_code('H',Infl,Code) :-
  member(Infl,[finite,infinitive]),
  member(Code,[';~cat_Vi',';~cat_Vt',';~cat_Ve']).
verb_code('H',Infl,Code) :-
  member(Infl,[imperative,ing_participle,en_participle]),
  member(Code,[';~cat_Vi',';~cat_Vt']).
verb_code('B',_,Code) :-
  member(Code,[
     ';~La',';~Ln',
     ';~I',
     ';~cat_Vt',';~cat_Vt_passive_',';~cat_Ve_passive_',
     ';~equ_Vf',';~equ_Vw',';~equ_Vt',';~equ_Vg'
    ]).
verb_code('B',Infl,Code) :-
  member(Infl,[finite,infinitive,ing_participle,en_participle]),
  member(Code,[';~ex_V',';~ex_Vp',';~ex_Vpr']).
verb_code('B',Infl,Code) :-
  member(Infl,[finite,infinitive,en_participle]),
  member(Code,[
     ';~cat_Vg',
     ';~ex_cat_Vt',';~ex_cat_Vt_passive_',';~ex_cat_Vg',';~ex_cat_Ve_passive_',
     ';~cleft_Vn'
    ]).

% Modal verbs

modal(';~cat_Vi',[node('MD;~cat_Vi',[node(Word,[])])|L],L) -->
  [w('MD',';~cat_Vi',Word)].
modal(';~cat_Vt',[node('MD;~cat_Vt',[node(Word,[])])|L],L) -->
  [w('MD',';~cat_Vt',Word)].

% Other clause level words

optional_clitic_negation([node('NEG;_clitic_',[node(Word,[])])|L],L) -->
  [w('NEG;_clitic_',Word)].
optional_clitic_negation([node('NEG;_clitic_;_high_',[node(Word,[])])|L],L) -->
  [w('NEG;_clitic_;_high_',Word)].
optional_clitic_negation(L,L) -->
  [].
neg([node('NEG;_high_',[node(Word,[])])|L],L) -->
  [w('NEG;_high_',Word)].
neg([node('NEG',[node(Word,[])])|L],L) -->
  [w('NEG',Word)].
to([node('TO',[node(Word,[])])|L],L) -->
  [w('TO',Word)].

initial_adverbial([node('CONJ;_cl_',[node(Word,[])])|L],L) -->
  [w('CONJ;_cl_',Word)].
initial_adverbial([node('INTJ',[node(Word,[])])|L],L) -->
  [w('INTJ',Word)].
initial_adverbial([node('REACT',[node(Word,[])])|L],L) -->
  [w('REACT',Word)].
initial_adverbial([node('FRM',[node(Word,[])])|L],L) -->
  [w('FRM',Word)].
initial_adverbial(L,L0) -->
  adverb_phrase('-NIM',non_interrogative,L,L0).
initial_adverbial(L,L0) -->
  preposition_phrase('-NIM',non_interrogative,L,L0).
initial_adverbial(L,L0) -->
  scon_clause(L,L0).

% Connective words

conj(node('CONJ',[node(Word,[])])) -->
  [w('CONJ',Word)].
comp([node('C',[node(Word,[])])|L],L) -->
  [w('C',Word)].
comp_wq([node('WQ',[node(Word,[])])|L],L) -->
  [w('WQ',Word)].
conn([node('P-CONN',[node(Word,[])])|L],L) -->
  [w('P-CONN',Word)].
role([node('P-ROLE',[node(Word,[])])|L],L) -->
  [w('P-ROLE',Word)].

% Punctuation

punc(final,[node('PUNC',[node('.',[])])|L],L) -->
  [w('PUNC','.')].
punc(final,[node('PUNC',[node('!',[])])|L],L) -->
  [w('PUNC','!')].
punc(final_question,[node('PUNC',[node('?',[])])|L],L) -->
  [w('PUNC','?')].
punc(non_final,[node('PUNC',[node(',',[])])|L],L) -->
  [w('PUNC',',')].
punc(left_quotation_mark,[node('PULQ',[node('<ldquo>',[])])|L],L) -->
  [w('PULQ','<ldquo>')].
punc(left_quotation_mark,[node('PULQ',[node('<lsquo>',[])])|L],L) -->
  [w('PULQ','<lsquo>')].
punc(right_quotation_mark,[node('PURQ',[node('<rsquo>',[])])|L],L) -->
  [w('PURQ','<rsquo>')].
punc(right_quotation_mark,[node('PURQ',[node('<rdquo>',[])])|L],L) -->
  [w('PURQ','<rdquo>')].

% Optional punctuation

optional_punc_non_final(L,L0) -->
  punc(non_final,L,L0).
optional_punc_non_final(L,L) -->
  [].

% Noun phrase

noun_phrase(Ext,Type,[node(Label,NL)|L],L) -->
  {
    atom_concat('NP',Ext,Label)
  },
  noun_phrase_top(Type,NL,[]).

noun_phrase_top(Type,L,L0) -->
  noun_head_full(Type,L,L0).
noun_phrase_top(Type,L,L0) -->
  noun_phrase_initial_layer(Type,L,L0).

noun_phrase_initial_layer(Type,L,L0) -->
  determiner_layer(Type,L,L1),
  internal_np_higher_layer(L1,L0).
noun_phrase_initial_layer(Type,L,L0) -->
  {
    member(Type,[interrogative,non_privileged])
  },
  adjective_phrase('',interrogative,L,L1),
  internal_np_higher_layer(L1,L0).
noun_phrase_initial_layer(Type,L,L0) -->
  {
    member(Type,[non_privileged,non_interrogative])
  },
  noun_head(L,L0).
noun_phrase_initial_layer(Type,L,L0) -->
  {
    member(Type,[non_privileged,non_interrogative])
  },
  internal_np_higher_layer(L,L0).
noun_phrase_initial_layer(Type,[node('NLYR',[CONJ1,NP1,node('CONJP',[CONJ2,NP2])])|L],L) -->
  conj(CONJ1),
  noun_phrase('',Type,[NP1],[]),
  conj(CONJ2),
  noun_phrase('',Type,[NP2],[]).
noun_phrase_initial_layer(Type,[node('NLYR',[NP|CL])|L],L) -->
  noun_phrase('',Type,[NP],[]),
  noun_phrase_initial_tail(Type,CL,[]).
noun_phrase_initial_layer(Type,L,L0) -->
  noun_phrase_initial_layer(Type,L,L1),
  preposition_phrase('',non_privileged,L1,L0).
noun_phrase_initial_layer(Type,L,L0) -->
  {
    Type == relative
  },
  noun_phrase_initial_layer(non_privileged,L,L1),
  preposition_phrase('',relative,L1,L0).
noun_phrase_initial_layer(Type,L,L0) -->
  noun_phrase_initial_layer(Type,L,L1),
  relative_clause(L1,L0).

noun_phrase_initial_tail(Type,[node('CONJP',[CONJ,NP])|L],L) -->
  conj(CONJ),
  noun_phrase('',Type,[NP],[]).
noun_phrase_initial_tail(Type,[PU,node('CONJP',[NP])|L],L0) -->
  punc(non_final,[PU],[]),
  noun_phrase('',Type,[NP],[]),
  noun_phrase_initial_tail(Type,L,L0).

internal_np_higher_layer(L,L0) -->
  adjective_phrase('',non_interrogative,L,L1),
  internal_np_higher_layer(L1,L0).
internal_np_higher_layer([node('IP-PPL',[node('VAG;~I',[node(Word,[])])])|L],L0) -->
  [w('VAG',';~I',Word)],
  internal_np_higher_layer(L,L0).
internal_np_higher_layer([node('IP-PPL',[node('NP-LGS',[node('*',[])]),node('VVN;~Tn',[node(Word,[])])])|L],L0) -->
  [w('VVN',';~Tn',Word)],
  internal_np_higher_layer(L,L0).
internal_np_higher_layer(L,L0) -->
  internal_np_lower_layer(L,L0).
internal_np_higher_layer(L,L0) -->
  internal_np_higher_layer(L,L1),
  preposition_phrase('',non_privileged,L1,L0).
internal_np_higher_layer(L,L0) -->
  internal_np_higher_layer(L,L1),
  relative_clause(L1,L0).
internal_np_higher_layer([node('NLYR',[node('NP',NL)|CL])|L],L) -->
  internal_np_higher_layer(NL,[]),
  internal_np_higher_tail(CL,[]).

internal_np_higher_tail([node('CONJP',[CONJ,node('NP',NL)])|L],L) -->
  conj(CONJ),
  internal_np_higher_layer(NL,[]).
internal_np_higher_tail([PU,node('CONJP',[node('NP',NL)])|L],L0) -->
  punc(non_final,[PU],[]),
  internal_np_higher_layer(NL,[]),
  internal_np_higher_tail(L,L0).

internal_np_lower_layer(L,L0) -->
  noun(L,L0).
internal_np_lower_layer(L,L0) -->
  noun(L,L1),
  internal_np_lower_layer(L1,L0).
internal_np_lower_layer([node('NLYR',[NL1,NL2|NL3])|L],L0) -->
  internal_np_higher_layer([NL1,NL2|NL3],[]),
  internal_np_lower_layer(L,L0).
internal_np_lower_layer([node('NLYR',[node('NLYR',[node('NP',NL)|CL])])|L],L0) -->
  internal_np_higher_layer(NL,[]),
  internal_np_higher_tail(CL,[]),
  internal_np_lower_layer(L,L0).
internal_np_lower_layer(L,L0) -->
  noun(L,L1),
  ip_to_inf('',[],L1,L0).
internal_np_lower_layer(L,L0) -->
  noun(L,L1),
  cp_that('',with_c,[],L1,L0).
internal_np_lower_layer(L,L0) -->
  noun(L,L1),
  cp_embedded_que('',[],L1,L0).
internal_np_lower_layer([node('NLYR',[node('NP',NL)|CL])|L],L) -->
  internal_np_lower_layer(NL,[]),
  internal_np_lower_tail(CL,[]).

internal_np_lower_tail([node('CONJP',[CONJ,node('NP',NL)])|L],L) -->
  conj(CONJ),
  internal_np_lower_layer(NL,[]).
internal_np_lower_tail([PU,node('CONJP',[node('NP',NL)])|L],L0) -->
  punc(non_final,[PU],[]),
  internal_np_lower_layer(NL,[]),
  internal_np_lower_tail(L,L0).

% Determiner layer

determiner_layer(Type,L,L0) -->
  det(Type,L,L0).
determiner_layer(Type,[node('NP-GENV',NL)|L],L) -->
  noun_phrase_genm_layer(Type,NL,[]).

% Genitive noun phrases

noun_phrase_genm_layer(Type,L,L0) -->
  pronoun_genm(Type,L,L0).
noun_phrase_genm_layer(Type,L,L0) -->
  noun_phrase_initial_layer(Type,L,L1),
  genm(L1,L0).
noun_phrase_genm_layer(Type,[node('NLYR',[node('NP',NL)|CL])|L],L) -->
  noun_phrase_genm_layer(Type,NL,[]),
  noun_phrase_genm_tail(Type,CL,[]).

noun_phrase_genm_tail(Type,[node('CONJP',[CONJ,node('NP',NL)])|L],L) -->
  conj(CONJ),
  noun_phrase_genm_layer(Type,NL,[]).
noun_phrase_genm_tail(Type,[PU,node('CONJP',[node('NP',NL)])|L],L0) -->
  punc(non_final,[PU],[]),
  noun_phrase_genm_layer(Type,NL,[]),
  noun_phrase_genm_tail(Type,L,L0).

% Adverb phrase

adverb_phrase(Ext,Type,[node(Label,AL)|L],L) -->
  {
    member(Type,[non_interrogative,interrogative,relative,particle]),
    atom_concat('ADVP',Ext,Label)
  },
  adverb_phrase_layer(Type,AL,[]).
adverb_phrase(Ext,Type1,[node(Label,AL)|L],L) -->
  {
    Type1 = non_privileged,
    member(Type2,[non_interrogative,interrogative]),
    atom_concat('ADVP',Ext,Label)
  },
  adverb_phrase_layer(Type2,AL,[]).

adverb_phrase_layer(Type,L,L0) -->
  adv(Type,L,L0).
adverb_phrase_layer(non_interrogative,L,L0) -->
  noun_phrase('',non_interrogative,L,L1),
  adverb_phrase_layer(non_interrogative,L1,L0).
adverb_phrase_layer(non_interrogative,L,L0) -->
  neg(L,L1),
  adverb_phrase_layer(non_interrogative,L1,L0).
adverb_phrase_layer(Type,L,L0) -->
  adverb_phrase('',Type,L,L1),
  adverb_phrase_layer(non_interrogative,L1,L0).
adverb_phrase_layer(non_interrogative,L,L0) -->
  adverb_phrase_layer(non_interrogative,L,L1),
  preposition_phrase('',non_interrogative,L1,L0).
adverb_phrase_layer(Type,[node('AVLYR',[ADVP|CL])|L],L) -->
  adverb_phrase('',Type,[ADVP],[]),
  adverb_phrase_tail(Type,CL,[]).

adverb_phrase_tail(Type,[node('CONJP',[CONJ,ADVP])|L],L) -->
  conj(CONJ),
  adverb_phrase('',Type,[ADVP],[]).
adverb_phrase_tail(Type,[PU,node('CONJP',[ADVP])|L],L0) -->
  punc(non_final,[PU],[]),
  adverb_phrase('',Type,[ADVP],[]),
  adverb_phrase_tail(Type,L,L0).

% Adjective phrase

adjective_phrase(Ext,Type,[node(Label,AL)|L],L) -->
  {
    member(Type,[catenative,non_interrogative,interrogative,relative]),
    atom_concat('ADJP',Ext,Label)
  },
  adjective_phrase_layer(Type,AL,[]).
adjective_phrase(Ext,Type1,[node(Label,AL)|L],L) -->
  {
    Type1 = non_privileged,
    member(Type2,[non_interrogative,interrogative]),
    atom_concat('ADJP',Ext,Label)
  },
  adjective_phrase_layer(Type2,AL,[]).

adjective_phrase_layer(Type,L,L0) -->
  {
    member(Type,[interrogative,relative])
  },
  adverb_phrase('',Type,L,L1),
  adjective_phrase_layer(non_interrogative,L1,L0).
adjective_phrase_layer(catenative,L,L0) -->
  adj(catenative,L,L1),
  ip_to_inf('',[],L1,L0).
adjective_phrase_layer(non_interrogative,L,L0) -->
  adj(non_interrogative,L,L0).
adjective_phrase_layer(non_interrogative,L,L0) -->
  adj(non_interrogative,L,L1),
  cp_that('',with_c,[],L1,L0).
adjective_phrase_layer(non_interrogative,L,L0) -->
  noun_phrase('',non_interrogative,L,L1),
  adjective_phrase_layer(non_interrogative,L1,L0).
adjective_phrase_layer(non_interrogative,L,L0) -->
  neg(L,L1),
  adjective_phrase_layer(non_interrogative,L1,L0).
adjective_phrase_layer(non_interrogative,L,L0) -->
  adverb_phrase('',non_interrogative,L,L1),
  adjective_phrase_layer(non_interrogative,L1,L0).
adjective_phrase_layer(non_interrogative,L,L0) -->
  adjective_phrase_layer(non_interrogative,L,L1),
  preposition_phrase('',non_interrogative,L1,L0).
adjective_phrase_layer(Type,[node('AJLYR',[ADJP|CL])|L],L) -->
  adjective_phrase('',Type,[ADJP],[]),
  adjective_phrase_tail(Type,CL,[]).

adjective_phrase_tail(Type,[node('CONJP',[CONJ,ADJP])|L],L) -->
  conj(CONJ),
  adjective_phrase('',Type,[ADJP],[]).
adjective_phrase_tail(Type,[PU,node('CONJP',[ADJP])|L],L0) -->
  punc(non_final,[PU],[]),
  adjective_phrase('',Type,[ADJP],[]),
  adjective_phrase_tail(Type,L,L0).

% Preposition phrases

preposition_phrase(Ext,Type,L,L0) -->
  preposition_phrase_layer(Type,PL,[]),
  {
    atom_concat('PP',Ext,Label),
    L = [node(Label,PL)|L0]
  }.
preposition_phrase(Ext,Type,L,L0) -->
  preposition_phrase('',Type,[PP1],[]),
  preposition_phrase_tail(Type,CL,[]),
  {
    atom_concat('PP',Ext,Label),
    L = [node(Label,[PP1|CL])|L0]
  }.

preposition_phrase_layer(Type,L,L0) -->
  role(L,L1),
  noun_phrase('',Type,L1,L0).
preposition_phrase_layer(Type,L,L0) -->
  role(L,L1),
  adverb_phrase('',Type,L1,L0).
preposition_phrase_layer(Type,L,L0) -->
  {
    member(Type,[non_privileged,non_interrogative])
  },
  role(L,[node('IP-PPL2;IP-PPL',IL)|L0]),
  ip_ppl_adverbial_layer(filled_sbj,ing_participle,IL,[]).
preposition_phrase_layer(Type,L,L0) -->
  {
    member(Type,[non_privileged,non_interrogative])
  },
  role(L,[node('IP-PPL3',IL)|L0]),
  ip_ppl_adverbial_layer(unfilled_sbj,ing_participle,IL,[]).
preposition_phrase_layer(Type,L,L0) -->
  {
    member(Type,[non_privileged,non_interrogative])
  },
  role(L,L1),
  cp_embedded_que('',[],L1,L0).
preposition_phrase_layer(Type,L,L0) -->
  {
    member(Type,[non_privileged,non_interrogative])
  },
  role(L,[node('IP-ADV',IL)|L0]),
  clause_top_layer(statement_order,[],IL,[]).

preposition_phrase_tail(Type,[node('CONJP',[CONJ,PP])|L],L) -->
  conj(CONJ),
  preposition_phrase('',Type,[PP],[]).
preposition_phrase_tail(Type,[PU,node('CONJP',[PP])|L],L0) -->
  punc(non_final,[PU],[]),
  preposition_phrase('',Type,[PP],[]),
  preposition_phrase_tail(Type,L,L0).

% Verb complements top layer

verb_complements_top_layer(Code,Store,SbjType,Voice,L,L0) -->
  verb_complements_by_code(Code,Store,SbjType,Voice,L,L0).

verb_complements_top_layer(Code,Store,provisional_sbj,Voice,L,L0) -->
  verb_complements_top_layer(Code,Store,filled_sbj,Voice,L,L1),
  notional_item('-NSBJ',L1,L0).
verb_complements_top_layer(';~La',Store,derived_sbj,active,L,L0) -->
  verb_complements_top_layer(';~La',[],filled_sbj,active,L,[node('IP-INF-NSBJ',VL)|L0]),
  {
    member(SbjType,[filled_sbj,unfilled_sbj])
  },
  to_inf_layer(Store,SbjType,passive,VL,[]).
verb_complements_top_layer(';~La',Store,derived_sbj,active,L,L0) -->
  verb_complements_top_layer(';~La',[],filled_sbj,active,L,[node('IP-INF-NSBJ',VL)|L0]),
  to_inf_layer(Store,filled_sbj,active,VL,[]).
verb_complements_top_layer(Code,Store,SbjType,Voice,L,L0) -->
  neg(L,L1),
  verb_complements_top_layer(Code,Store,SbjType,Voice,L1,L0).
verb_complements_top_layer(Code,Store,SbjType,Voice,L,L0) -->
  adverbial(L,L1),
  verb_complements_top_layer(Code,Store,SbjType,Voice,L1,L0).
verb_complements_top_layer(Code,Store,SbjType,Voice,L,L0) -->
  verb_complements_top_layer(Code,Store,SbjType,Voice,L,L1),
  adverbial(L1,L0).

% Adverbials

adverbial(L,L0) -->
  adverb_phrase('-NIM',non_privileged,L,L0).
adverbial(L,L0) -->
  preposition_phrase('-NIM',non_privileged,L,L0).
adverbial(L,L0) -->
  scon_clause(L,L0).

% Linking verb complements

verb_complements_by_code(';~La',[],filled_sbj,active,L,L0) -->
  {
    member(Type,[catenative,non_privileged])
  },
  adjective_phrase('-PRD2',Type,L,L0).
verb_complements_by_code(';~La',[],expletive_sbj,active,L,L0) -->
  adjective_phrase('-PRD2',non_privileged,L,L0).
verb_complements_by_code(';~La',[adjp(ICH)],filled_sbj,active,[node('ADJP-PRD2',[ICH])|L],L) -->
  [].

verb_complements_by_code(';~Ln',[],filled_sbj,active,L,L0) -->
  noun_phrase('-PRD2',non_privileged,L,L0).
verb_complements_by_code(';~Ln',[],expletive_sbj,active,L,L0) -->
  noun_phrase('-PRD2',non_privileged,L,L0).
verb_complements_by_code(';~Ln',[np(ICH)],filled_sbj,active,[node('NP-PRD2',[ICH])|L],L) -->
  [].

% Intransitive verb complements

verb_complements_by_code(';~I',[],filled_sbj,active,L,L) -->
  [].
verb_complements_by_code(';~I',[],expletive_sbj,active,L,L) -->
  [].

verb_complements_by_code(';~Ip',[],filled_sbj,active,L,L0) -->
  adverb_phrase('-CLR',particle,L,L0).
verb_complements_by_code(';~Ip',[advp(ICH)],filled_sbj,active,[node('ADVP-CLR',[ICH])|L],L) -->
  [].

verb_complements_by_code(';~Ipr',[],filled_sbj,active,L,L0) -->
  preposition_phrase('-CLR',non_privileged,L,L0).
verb_complements_by_code(';~Ipr',[np(ICH)],filled_sbj,active,[node('PP-CLR',[Role,node('NP',[ICH])])|L],L) -->
  role([Role],[]).
verb_complements_by_code(';~Ipr',[pp(ICH)],filled_sbj,active,[node('PP-CLR',[ICH])|L],L) -->
  [].
verb_complements_by_code(';~Ipr',[],filled_sbj,passive,[node('PP-CLR',[Role])|L],L) -->
  role([Role],[]).

verb_complements_by_code(';~In/pr',[],filled_sbj,active,L,L0) -->
  noun_phrase('-CLR',non_privileged,L,L0).
verb_complements_by_code(';~In/pr',[],filled_sbj,active,L,L0) -->
  preposition_phrase('-CLR',non_privileged,L,L0).

verb_complements_by_code(';~It',[],filled_sbj,active,L,L0) -->
  ip_to_inf('-CLR',[],L,L0).

% Mono-transitive verb complements

verb_complements_by_code(';~Tn',[],filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,L0).
verb_complements_by_code(';~Tn',[np(ICH)],filled_sbj,active,[node('NP-OB1',[ICH])|L],L) -->
  [].
verb_complements_by_code(';~Tn',[],filled_sbj,passive,L,L) -->
  [].

verb_complements_by_code(';~Tn.p',[],filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,L1),
  adverb_phrase('-CLR',particle,L1,L0).
verb_complements_by_code(';~Tn.p',[],filled_sbj,active,L,L0) -->
  adverb_phrase('-CLR',particle,L,L1),
  noun_phrase('-OB1',non_privileged,L1,L0).
verb_complements_by_code(';~Tn.p',[np(ICH)],filled_sbj,active,[node('NP-OB1',[ICH])|L],L0) -->
  adverb_phrase('-CLR',particle,L,L0).
verb_complements_by_code(';~Tn.p',[advp(ICH)],filled_sbj,active,[node('ADVP-CLR',[ICH])|L],L0) -->
  noun_phrase('-OB1',non_privileged,L,L0).
verb_complements_by_code(';~Tn.p',[advp(ICH2),np(ICH1)],filled_sbj,active,[node('NP-OB1',[ICH1]),node('ADVP-CLR',[ICH2])|L],L) -->
  [].
verb_complements_by_code(';~Tn.p',[],filled_sbj,passive,L,L0) -->
  adverb_phrase('-CLR',particle,L,L0).
verb_complements_by_code(';~Tn.p',[advp(ICH)],filled_sbj,passive,[node('ADVP-CLR',[ICH])|L],L) -->
  [].

verb_complements_by_code(';~Tn.pr',[],filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,L1),
  preposition_phrase('-CLR',non_privileged,L1,L0).
verb_complements_by_code(';~Tn.pr',[],filled_sbj,active,L,L0) -->
  preposition_phrase('-CLR',non_privileged,L,L1),
  noun_phrase('-OB1',non_privileged,L1,L0).
verb_complements_by_code(';~Tn.pr',[],filled_sbj,active,[node('NP-OB1',[node('PRO;_provisional_',[node(Word,[])])])|L],L0) -->
  [w('PRO;_provisional_',Word)],
  preposition_phrase('-CLR',non_privileged,L,L1),
  notional_item('-NOB1',L1,L0).
verb_complements_by_code(';~Tn.pr',[np(ICH)],filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,[node('PP-CLR',[Role,node('NP',[ICH])])|L0]),
  role([Role],[]).
verb_complements_by_code(';~Tn.pr',[pp(ICH)],filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,[node('PP-CLR',[ICH])|L0]).
verb_complements_by_code(';~Tn.pr',[np(ICH)],filled_sbj,active,[node('NP-OB1',[ICH])|L],L0) -->
  preposition_phrase('-CLR',non_privileged,L,L0).
verb_complements_by_code(';~Tn.pr',[],filled_sbj,passive,L,L0) -->
  preposition_phrase('-CLR',non_privileged,L,L0).
verb_complements_by_code(';~Tn.pr',[np(ICH)],filled_sbj,passive,[node('PP-CLR',[Role,node('NP',[ICH])])|L],L) -->
  role([Role],[]).
verb_complements_by_code(';~Tn.pr',[pp(ICH)],filled_sbj,passive,[node('PP-CLR',[ICH])|L],L) -->
  [].

verb_complements_by_code(';~Tf',Store,filled_sbj,active,L,L0) -->
  cp_that('-OB1',_,Store,L,L0).

verb_complements_by_code(';~Tw',Store,filled_sbj,active,L,L0) -->
  cp_embedded_que('-OB1',Store,L,L0).

verb_complements_by_code(';~Tr',[],filled_sbj,active,L,L0) -->
  optional_punc_non_final(L,L3),
  punc(left_quotation_mark,L3,L2),
  utterance('-OB1',L2,L1),
  punc(right_quotation_mark,L1,L0).
verb_complements_by_code(';~Tr',[utterance(ICH)],filled_sbj,active,[node('utterance-OB1',[ICH])|L],L) -->
  [].

verb_complements_by_code(';~Tt',Store,filled_sbj,active,L,L0) -->
  ip_to_inf('-OB1',Store,L,L0).

verb_complements_by_code(';~Tnt',Store,filled_sbj,active,L,L0) -->
  noun_phrase('-DOB1',non_privileged,L,[node('IP-INF-OB1',VL)|L0]),
  to_inf_layer(Store,filled_sbj,active,VL,[]).
verb_complements_by_code(';~Tnt',Store,filled_sbj,active,[node('NP-DOB1',[node('PRO;_expletive_',[node(Word,[])])]),node('IP-INF-OB1',VL)|L],L) -->
  [w('PRO;_expletive_',Word)],
  to_inf_layer(Store,filled_sbj,active,VL,[]).
verb_complements_by_code(';~Tnt',[np(ICH)],filled_sbj,active,[node('NP-DOB1',[ICH]),node('IP-INF-OB1',VL)|L],L) -->
  to_inf_layer([],filled_sbj,active,VL,[]).
verb_complements_by_code(';~Tnt',Store,filled_sbj,passive,[node('IP-INF-OB1',VL)|L],L) -->
  to_inf_layer(Store,filled_sbj,active,VL,[]).
verb_complements_by_code(';~Tnt',Store,filled_sbj,active,[node('NP-DOB1',[node('PRO;_provisional_',[node(Word,[])])]),node('IP-INF-OB1',VL)|L],L0) -->
  [w('PRO;_provisional_',Word)],
  to_inf_layer(Store,filled_sbj,active,VL,[]),
  notional_item('-NOB1',L,L0).

verb_complements_by_code(';~Tni',[],filled_sbj,active,L,L0) -->
  noun_phrase('-DOB1',non_privileged,L,[node('IP-INF-OB1',VL)|L0]),
  verb_phrase_layer([],filled_sbj,infinitive,active,VL,[]).
verb_complements_by_code(';~Tni',[np(ICH)],filled_sbj,active,[node('NP-DOB1',[ICH]),node('IP-INF-OB1',VL)|L],L) -->
  verb_phrase_layer([],filled_sbj,infinitive,active,VL,[]).

verb_complements_by_code(';~Tg',Store,filled_sbj,active,L,L0) -->
  ip_ppl_active('-OB1',Store,filled_sbj,ing_participle,L,L0).

verb_complements_by_code(';~Tng',[],filled_sbj,active,L,L0) -->
  noun_phrase('-DOB1',non_privileged,L,L1),
  ip_ppl_active('-OB1',[],filled_sbj,ing_participle,L1,L0).
verb_complements_by_code(';~Tng',[np(ICH)],filled_sbj,active,[node('NP-DOB1',[ICH])|L],L0) -->
  ip_ppl_active('-OB1',[],filled_sbj,ing_participle,L,L0).
verb_complements_by_code(';~Tng',[],filled_sbj,passive,L,L0) -->
  ip_ppl_active('-OB1',[],filled_sbj,ing_participle,L,L0).

verb_complements_by_code(';~Tsg',[],filled_sbj,active,[node('IP-PPL3-OB1',[node('NP-SBJ',NL)|VL])|L],L) -->
  noun_phrase_genm_layer(non_privileged,NL,[]),
  verb_phrase_layer([],filled_sbj,ing_participle,active,VL,[]).
verb_complements_by_code(';~Tsg',[],filled_sbj,active,[node('IP-PPL3-OB1',[node('NP-SBJ',NL)|VL])|L],L) -->
  noun_phrase_top(non_privileged,NL,[]),
  verb_phrase_layer([],filled_sbj,ing_participle,active,VL,[]).

% Ditransitive verb complements

verb_complements_by_code(';~Dn.n',[],filled_sbj,active,L,L0) -->
  noun_phrase('-OB2',non_privileged,L,L1),
  noun_phrase('-OB1',non_privileged,L1,L0).
verb_complements_by_code(';~Dn.n',[np(ICH)],filled_sbj,active,[node('NP-OB1',[ICH])|L],L0) -->
  noun_phrase('-OB2',non_privileged,L,L0).
verb_complements_by_code(';~Dn.n',[np(ICH)],filled_sbj,active,[node('NP-OB2',[ICH])|L],L0) -->
  noun_phrase('-OB1',non_privileged,L,L0).
verb_complements_by_code(';~Dn.n',[],filled_sbj,passive,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,L0).
verb_complements_by_code(';~Dn.n',[np(ICH)],filled_sbj,passive,[node('NP-OB1',[ICH])|L],L) -->
  [].

verb_complements_by_code(';~Dn.f',Store,filled_sbj,active,L,L0) -->
  noun_phrase('-OB2',non_privileged,L,L1),
  cp_that('-OB1',_,Store,L1,L0).
verb_complements_by_code(';~Dn.f',Store,filled_sbj,passive,L,L0) -->
  cp_that('-OB1',_,Store,L,L0).

verb_complements_by_code(';~Dn.w',Store,filled_sbj,active,L,L0) -->
  noun_phrase('-OB2',non_privileged,L,L1),
  cp_embedded_que('-OB1',Store,L1,L0).
verb_complements_by_code(';~Dn.w',Store,filled_sbj,passive,L,L0) -->
  cp_embedded_que('-OB1',Store,L,L0).

verb_complements_by_code(';~Dn.r',[],filled_sbj,active,L,L0) -->
  noun_phrase('-OB2',non_privileged,L,L4),
  optional_punc_non_final(L4,L3),
  punc(left_quotation_mark,L3,L2),
  utterance('-OB1',L2,L1),
  punc(right_quotation_mark,L1,L0).
verb_complements_by_code(';~Dn.r',[],filled_sbj,passive,L,L0) -->
  optional_punc_non_final(L,L3),
  punc(left_quotation_mark,L3,L2),
  utterance('-OB1',L2,L1),
  punc(right_quotation_mark,L1,L0).

verb_complements_by_code(';~Dn.t',Store,filled_sbj,active,L,L0) -->
  noun_phrase('-OB2',non_privileged,L,[node('IP-INF-OB1',VL)|L0]),
  to_inf_layer(Store,filled_sbj,active,VL,[]).
verb_complements_by_code(';~Dn.t',Store,filled_sbj,passive,[node('IP-INF-OB1',VL)|L],L) -->
  to_inf_layer(Store,filled_sbj,active,VL,[]).

verb_complements_by_code(';~Dn.*',[],filled_sbj,active,L,L0) -->
  noun_phrase('-OB2',non_privileged,L,L0).

verb_complements_by_code(';~Dpr.n',[],filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,L1),
  preposition_phrase('-OB2',non_privileged,L1,L0).
verb_complements_by_code(';~Dpr.n',[],filled_sbj,active,L,L0) -->
  preposition_phrase('-OB2',non_privileged,L,L1),
  noun_phrase('-OB1',non_privileged,L1,L0).
verb_complements_by_code(';~Dpr.n',[np(ICH)],filled_sbj,active,[node('NP-OB1',[ICH])|L],L0) -->
  preposition_phrase('-OB2',non_privileged,L,L0).
verb_complements_by_code(';~Dpr.n',[pp(ICH)],filled_sbj,active,[node('PP-OB2',[ICH])|L],L0) -->
  noun_phrase('-OB1',non_privileged,L,L0).
verb_complements_by_code(';~Dpr.n',[],filled_sbj,passive,L,L0) -->
  preposition_phrase('-OB2',non_privileged,L,L0).

verb_complements_by_code(';~Dpr.f',Store,filled_sbj,active,L,L0) -->
  preposition_phrase('-OB2',non_privileged,L,L1),
  cp_that('-OB1',_,Store,L1,L0).

verb_complements_by_code(';~Dpr.r',[],filled_sbj,active,L,L0) -->
  preposition_phrase('-OB2',non_privileged,L,L4),
  optional_punc_non_final(L4,L3),
  punc(left_quotation_mark,L3,L2),
  utterance('-OB1',L2,L1),
  punc(right_quotation_mark,L1,L0).

% Complex-transitive verb complements

verb_complements_by_code(';~Cn.a',[],filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,L1),
  adjective_phrase('-PRD',non_privileged,L1,L0).
verb_complements_by_code(';~Cn.a',[],filled_sbj,active,L,L0) -->
  adjective_phrase('-PRD',non_privileged,L,L1),
  noun_phrase('-OB1',non_privileged,L1,L0).
verb_complements_by_code(';~Cn.a',[np(ICH)],filled_sbj,active,[node('NP-OB1',[ICH])|L],L0) -->
  adjective_phrase('-PRD',non_privileged,L,L0).
verb_complements_by_code(';~Cn.a',[],filled_sbj,active,[node('NP-OB1',[node('PRO;_provisional_',[node(Word,[])])])|L],L0) -->
  [w('PRO;_provisional_',Word)],
  adjective_phrase('-PRD',non_privileged,L,L1),
  notional_item('-NOB1',L1,L0).
verb_complements_by_code(';~Cn.a',Store,filled_sbj,active,L,L0) -->
  noun_phrase('-DOB1',non_privileged,L,L2),
  adjective_phrase('-PRD',non_privileged,L2,[node('IP-INF-NOB1',VL)|L0]),
  to_inf_layer(Store,filled_sbj,passive,VL,[]).
verb_complements_by_code(';~Cn.a',[np(ICH)],filled_sbj,active,[node('NP-DOB1',[ICH])|L],L0) -->
  adjective_phrase('-PRD',non_privileged,L,[node('IP-INF-NOB1',VL)|L0]),
  to_inf_layer([],filled_sbj,passive,VL,[]).
verb_complements_by_code(';~Cn.a',[],filled_sbj,passive,L,L0) -->
  adjective_phrase('-PRD',non_privileged,L,L0).
verb_complements_by_code(';~Cn.a',[],filled_sbj,passive,L,L0) -->
  adjective_phrase('-PRD',non_privileged,L,[node('IP-INF-NOB1',VL)|L0]),
  to_inf_layer([],filled_sbj,passive,VL,[]).

verb_complements_by_code(';~Cn.n',[],filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,L1),
  noun_phrase('-PRD',non_privileged,L1,L0).
verb_complements_by_code(';~Cn.n',[],filled_sbj,active,[node('NP-OB1',[node('PRO;_provisional_',[node(Word,[])])])|L],L0) -->
  [w('PRO;_provisional_',Word)],
  noun_phrase('-PRD',non_privileged,L,L1),
  notional_item('-NOB1',L1,L0).
verb_complements_by_code(';~Cn.n',[np(ICH)],filled_sbj,active,[node('NP-OB1',[ICH])|L],L0) -->
  noun_phrase('-PRD',non_privileged,L,L0).
verb_complements_by_code(';~Cn.n',[],filled_sbj,passive,L,L0) -->
  noun_phrase('-PRD',non_privileged,L,L0).
verb_complements_by_code(';~Cn.n',[np(ICH)],filled_sbj,passive,[node('NP-PRD',[ICH])|L],L) -->
  [].

verb_complements_by_code(';~Cn.n/a',[],filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,L1),
  preposition_phrase('-PRD',non_privileged,L1,L0).
verb_complements_by_code(';~Cn.n/a',[],filled_sbj,passive,L,L0) -->
  preposition_phrase('-PRD',non_privileged,L,L0).

verb_complements_by_code(';~Cn.pr',[],filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,L1),
  preposition_phrase('-PRD',non_privileged,L1,L0).
verb_complements_by_code(';~Cn.pr',[],filled_sbj,passive,L,L0) -->
  preposition_phrase('-PRD',non_privileged,L,L0).

verb_complements_by_code(';~Cn.t',Store,filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,[node('IP-INF-PRD',VL)|L0]),
  to_inf_layer(Store,filled_sbj,active,VL,[]).
verb_complements_by_code(';~Cn.t',[np(ICH)],filled_sbj,active,[node('NP-OB1',[ICH]),node('IP-INF-PRD',VL)|L],L) -->
  to_inf_layer([],filled_sbj,active,VL,[]).
verb_complements_by_code(';~Cn.t',Store,filled_sbj,passive,[node('IP-INF-PRD',VL)|L],L) -->
  to_inf_layer(Store,filled_sbj,active,VL,[]).

verb_complements_by_code(';~Cn.i',Store,filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,[node('IP-INF-PRD',VL)|L0]),
  verb_phrase_layer(Store,filled_sbj,infinitive,active,VL,[]).

verb_complements_by_code(';~Cn.g',Store,filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,L1),
  ip_ppl_active('-PRD',Store,filled_sbj,ing_participle,L1,L0).
verb_complements_by_code(';~Cn.g',Store,filled_sbj,passive,L,L0) -->
  ip_ppl_active('-PRD',Store,filled_sbj,ing_participle,L,L0).

% VP24A

verb_complements_by_code(';~VP24A',Store,filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,L1),
  ip_ppl_passive('-PRD',Store,filled_sbj,en_participle,L1,L0).
verb_complements_by_code(';~VP24A',Store,filled_sbj,passive,L,L0) -->
  ip_ppl_passive('-PRD',Store,filled_sbj,en_participle,L,L0).

% VP24B

verb_complements_by_code(';~VP24B',Store,filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,L1),
  ip_ppl_passive('-PRD',Store,filled_sbj,en_participle,L1,L0).

% VP24C

verb_complements_by_code(';~VP24C',Store,filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,L1),
  ip_ppl_passive('-PRD',Store,filled_sbj,en_participle,L1,L0).

% Existential verb complements

verb_complements_by_code(';~ex_V',[],there_sbj,active,L,L0) -->
  noun_phrase('-ESBJ',non_privileged,L,L0).
verb_complements_by_code(';~ex_V',[np(ICH)],there_sbj,active,[node('NP-ESBJ',[ICH])|L],L) -->
  [].

verb_complements_by_code(';~ex_Vp',[],there_sbj,active,L,L0) -->
  noun_phrase('-ESBJ',non_privileged,L,L1),
  adverb_phrase('-CLR',particle,L1,L0).
verb_complements_by_code(';~ex_Vp',[np(ICH)],there_sbj,active,[node('NP-ESBJ',[ICH])|L],L0) -->
  adverb_phrase('-CLR',particle,L,L0).

verb_complements_by_code(';~ex_Vpr',[],there_sbj,active,L,L0) -->
  noun_phrase('-ESBJ',non_privileged,L,L1),
  preposition_phrase('-CLR',non_privileged,L1,L0).

verb_complements_by_code(';~ex_cat_Vt',Store,there_sbj,active,L,L0) -->
  noun_phrase('-ESBJ',non_privileged,L,[node('IP-INF-CAT',VL)|L0]),
  to(VL,VL1),
  verb_phrase_layer(Store,filled_sbj,infinitive,active,VL1,[]).
verb_complements_by_code(';~ex_cat_Vt',[np(ICH)|Store],there_sbj,active,[node('NP-ESBJ',[ICH]),node('IP-INF-CAT',VL)|L],L) -->
  to(VL,VL1),
  verb_phrase_layer(Store,filled_sbj,infinitive,active,VL1,[]).

verb_complements_by_code(';~ex_cat_Vt_passive_',Store,there_sbj,active,L,L0) -->
  noun_phrase('-ESBJ',non_privileged,L,[node('IP-INF-CAT',VL)|L0]),
  {
    member(SbjType,[filled_sbj,unfilled_sbj])
  },
  to_inf_layer(Store,SbjType,passive,VL,[]).
verb_complements_by_code(';~ex_cat_Vt_passive_',[np(ICH)|Store],there_sbj,active,[node('NP-ESBJ',[ICH]),node('IP-INF-CAT',VL)|L],L) -->
  {
    member(SbjType,[filled_sbj,unfilled_sbj])
  },
  to_inf_layer(Store,SbjType,passive,VL,[]).

verb_complements_by_code(';~ex_cat_Vg',Store,there_sbj,active,L,L0) -->
  noun_phrase('-ESBJ',non_privileged,L,L1),
  ip_ppl_active('-CAT',Store,filled_sbj,ing_participle,L1,L0).
verb_complements_by_code(';~ex_cat_Vg',[np(ICH)|Store],there_sbj,active,[node('NP-ESBJ',[ICH])|L],L0) -->
  ip_ppl_active('-CAT',Store,filled_sbj,ing_participle,L,L0).

verb_complements_by_code(';~ex_cat_Ve_passive_',Store,there_sbj,active,L,L0) -->
  noun_phrase('-ESBJ',non_privileged,L,L1),
  ip_ppl_passive('-CAT',Store,filled_sbj,en_participle,L1,L0).
verb_complements_by_code(';~ex_cat_Ve_passive_',[np(ICH)|Store],there_sbj,active,[node('NP-ESBJ',[ICH])|L],L0) -->
  ip_ppl_passive('-CAT',Store,filled_sbj,en_participle,L,L0).

% Equatives

verb_complements_by_code(';~equ_Vf',[],filled_sbj,active,L,L0) -->
  cp_that('-PRD2',_,[],L,L0).

verb_complements_by_code(';~equ_Vw',[],filled_sbj,active,L,L0) -->
  cp_embedded_que('-PRD2',[],L,L0).

verb_complements_by_code(';~equ_Vt',[],filled_sbj,active,L,L0) -->
  ip_to_inf('-PRD2',[],L,L0).

verb_complements_by_code(';~equ_Vg',[],filled_sbj,active,[node('IP-PPL-PRD2',IL)|L],L) -->
  ip_ppl_adverbial_layer(filled_sbj,ing_participle,IL,[]).
verb_complements_by_code(';~equ_Vg',[],filled_sbj,active,[node('IP-PPL3-PRD2',IL)|L],L) -->
  ip_ppl_adverbial_layer(unfilled_sbj,ing_participle,IL,[]).

% Cleft

verb_complements_by_code(';~cleft_Vn',[],cleft_sbj,active,L,L0) -->
  noun_phrase('-FOC',non_privileged,L,[node('IP-CLF',IL)|L0]),
  relative_clause_finite_top_layer(IL,[]).
verb_complements_by_code(';~cleft_Vn',[np(ICH)],cleft_sbj,active,[node('NP-FOC',[ICH]),node('IP-CLF',IL)|L],L) -->
  relative_clause_finite_top_layer(IL,[]).

% V_as_though/as_if/like

verb_complements_by_code(';~V_as_though/as_if/like',Store,there_sbj,active,[node('PP-CLR',PL)|L],L) -->
  role(PL,[node('IP-ADV',IL)]),
  subject(there_sbj,IL,L1),
  clause_middle_layer(Store,there_sbj,L1,[]).
verb_complements_by_code(';~V_as_though/as_if/like',Store,expletive_sbj,active,[node('PP-CLR',PL)|L],L) -->
  role(PL,[node('IP-ADV',IL)]),
  clause_top_layer(statement_order,Store,IL,[]).
verb_complements_by_code(';~V_as_though/as_if/like',Store,filled_sbj,active,[node('PP-CLR',PL)|L],L) -->
  role(PL,[node('IP-ADV',IL)]),
  clause_top_layer(statement_order,Store,IL,[]).

% Catenative verb complements

verb_complements_by_code(';~cat_Vt',Store,SbjType,active,[node('IP-INF-CAT',VL)|L],L) -->
  to(VL,VL1),
  verb_phrase_layer(Store,SbjType,infinitive,active,VL1,[]).

verb_complements_by_code(';~cat_Vt_passive_',Store,filled_sbj,active,[node('IP-INF-CAT',VL)|L],L) -->
  {
    member(SbjType,[filled_sbj,unfilled_sbj])
  },
  to_inf_layer(Store,SbjType,passive,VL,[]).

verb_complements_by_code(';~cat_Vi',Store,SbjType,active,[node('IP-INF-CAT',VL)|L],L) -->
  verb_phrase_layer(Store,SbjType,infinitive,active,VL,[]).

verb_complements_by_code(';~cat_Vg',Store,SbjType,active,L,L0) -->
  ip_ppl_active('-CAT',Store,SbjType,ing_participle,L,L0).

verb_complements_by_code(';~cat_Vg_passive_',Store,SbjType,active,L,L0) -->
  ip_ppl_passive('-CAT',Store,SbjType,ing_participle,L,L0).

verb_complements_by_code(';~cat_Ve',Store,SbjType,active,L,L0) -->
  ip_ppl_active('-CAT',Store,SbjType,en_participle,L,L0).

verb_complements_by_code(';~cat_Ve_passive_',Store,SbjType,active,L,L0) -->
  ip_ppl_passive('-CAT',Store,SbjType,en_participle,L,L0).

% Clause top layer

clause_top_layer(statement_order,Store,L,L0) -->
  subject(SbjType,L,L1),
  clause_middle_layer(Store,SbjType,L1,L0).
clause_top_layer(imperative_clause,Store,L,L0) -->
  verb_phrase_layer(Store,filled_sbj,imperative,active,L,L0).
clause_top_layer(tag_question,[],L,L0) -->
  do_operator_layer(L,L1),
  {
    member(SbjType,[filled_sbj,expletive_sbj])
  },
  subject(SbjType,L1,L0).
clause_top_layer(tag_question,[],L,L0) -->
  have_be_or_md_finite_layer(_,L,L1),
  {
    member(SbjType,[filled_sbj,expletive_sbj])
  },
  subject(SbjType,L1,L0).
clause_top_layer(Type,Store,L,L0) -->
  {
    member(Type,[matrix_interrogative,matrix_constituent_interrogative])
  },
  do_operator_layer(L,L2),
  subject(SbjType,L2,L1),
  verb_phrase_layer(Store,SbjType,do_supported_infinitive,active,L1,L0).
clause_top_layer(Type,Store,L,L0) -->
  {
    member(Type,[matrix_interrogative,matrix_constituent_interrogative])
  },
  have_be_or_md_finite_layer(Code,L,L2),
  subject(SbjType,L2,L1),
  verb_complements_top_layer(Code,Store,SbjType,active,L1,L0).
clause_top_layer(matrix_constituent_interrogative,[np(ICH)|Store],[node('NP-SBJ',[ICH])|L],L0) -->
  {
    member(SbjType,[filled_sbj,derived_sbj])
  },
  clause_middle_layer(Store,SbjType,L,L0).

clause_top_layer(Type,Store,L,L0) -->
  initial_adverbial(L,L2),
  optional_punc_non_final(L2,L1),
  clause_top_layer(Type,Store,L1,L0).
clause_top_layer(Type,Store,L,L0) -->
  clause_top_layer(Type,Store,L,L2),
  optional_punc_non_final(L2,L1),
  adverbial(L1,L0).
clause_top_layer(matrix_interrogative,Store,L,L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  noun_phrase(Index,interrogative,L,L1),
  clause_top_layer(matrix_constituent_interrogative,[np(node(ICH,[]))|Store],L1,L0).
clause_top_layer(matrix_interrogative,Store,L,L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  preposition_phrase(Index,interrogative,L,L1),
  clause_top_layer(matrix_constituent_interrogative,[pp(node(ICH,[]))|Store],L1,L0).
clause_top_layer(matrix_interrogative,Store,L,L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  adjective_phrase(Index,interrogative,L,L1),
  clause_top_layer(matrix_constituent_interrogative,[adjp(node(ICH,[]))|Store],L1,L0).
clause_top_layer(matrix_interrogative,Store,L,L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  adverb_phrase(Index,interrogative,L,L1),
  clause_top_layer(matrix_constituent_interrogative,[advp(node(ICH,[]))|Store],L1,L0).
clause_top_layer(matrix_interrogative,Store,L,L0) -->
  adverb_phrase('-NIM',interrogative,L,L1),
  clause_top_layer(matrix_constituent_interrogative,Store,L1,L0).
clause_top_layer(statement_order,Store,L,L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  noun_phrase(Index,non_interrogative,L,L2),
  optional_punc_non_final(L2,L1),
  clause_top_layer(statement_order,[np(node(ICH,[]))|Store],L1,L0).
clause_top_layer(statement_order,Store,L,L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  preposition_phrase(Index,non_interrogative,L,L2),
  optional_punc_non_final(L2,L1),
  clause_top_layer(statement_order,[pp(node(ICH,[]))|Store],L1,L0).
clause_top_layer(statement_order,Store,L,L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  punc(left_quotation_mark,L,L4),
  utterance(Index,L4,L3),
  punc(right_quotation_mark,L3,L2),
  optional_punc_non_final(L2,L1),
  clause_top_layer(statement_order,[utterance(node(ICH,[]))|Store],L1,L0).
clause_top_layer(Type,Store,[node('ILYR',[node('ILYR',IL)|CL])|L],L) -->
  clause_top_layer(Type,Store,IL,[]),
  clause_top_tail(Type,Store,CL,[]).

clause_top_tail(Type,Store,[node('CONJP',[CONJ,node('ILYR',IL)])|L],L) -->
  conj(CONJ),
  clause_top_layer(Type,Store,IL,[]).
clause_top_tail(Type,Store,[PU,node('CONJP',[node('ILYR',IL)])|L],L0) -->
  punc(non_final,[PU],[]),
  clause_top_layer(Type,Store,IL,[]),
  clause_top_tail(Type,Store,L,L0).

% Verb phrase layer

verb_phrase_layer(Store,SbjType,Infl,Voice,L,L0) -->
  verb(Infl,Code,L,L1),
  verb_complements_top_layer(Code,Store,SbjType,Voice,L1,L0).
verb_phrase_layer(Store,SbjType,Infl,Voice,[node('DO',[node(Word,[])])|L],L0) -->
  {
    SbjType == filled_sbj,
    Infl == imperative,
    Voice == active
  },
  [w('DO','',Word)],
  optional_clitic_negation(L,L1),
  verb_phrase_layer(Store,SbjType,infinitive,Voice,L1,L0).
verb_phrase_layer(Store,SbjType,Infl,Voice,L,L0) -->
  adverb_phrase('-NIM',non_privileged,L,L1),
  verb_phrase_layer(Store,SbjType,Infl,Voice,L1,L0).
verb_phrase_layer(Store,SbjType,Infl,Voice,L,L0) -->
  verb_phrase_layer(Store,SbjType,Infl,Voice,L,L1),
  adverbial(L1,L0).
verb_phrase_layer(Store,SbjType,Infl,Voice,[node('ILYR',[node('ILYR',IL)|CL])|L],L) -->
  verb_phrase_layer(Store,SbjType,Infl,Voice,IL,[]),
  verb_phrase_tail(Store,SbjType,Infl,Voice,CL,[]).

verb_phrase_tail(Store,SbjType,Infl,Voice,[node('CONJP',[CONJ,node('ILYR',IL)])|L],L) -->
  conj(CONJ),
  verb_phrase_layer(Store,SbjType,Infl,Voice,IL,[]).
verb_phrase_tail(Store,SbjType,Infl,Voice,[PU,node('CONJP',[node('ILYR',IL)])|L],L0) -->
  punc(non_final,[PU],[]),
  verb_phrase_layer(Store,SbjType,Infl,Voice,IL,[]),
  verb_phrase_tail(Store,SbjType,Infl,Voice,L,L0).

% DO operator layer

do_operator_layer([node(Tag,[node(Word,[])])|L],L0) -->
  [w(Tag,'',Word)],
  {
    member(Tag,['DOP','DOD'])
  },
  optional_clitic_negation(L,L0).

% HAVE, BE or modal verb finite layer

have_be_or_md_finite_layer(Code,[node(TagCode,[node(Word,[])])|L],L0) -->
  [w(Tag,Code,Word)],
  {
    member(Tag,['HVP','HVD']),
    verb_code('H',finite,Code),
    atom_concat(Tag,Code,TagCode)
  },
  optional_clitic_negation(L,L0).
have_be_or_md_finite_layer(Code,[node(TagCode,[node(Word,[])])|L],L0) -->
  [w(Tag,Code,Word)],
  {
    member(Tag,['BEP','BED']),
    verb_code('B',finite,Code),
    atom_concat(Tag,Code,TagCode)
  },
  optional_clitic_negation(L,L0).
have_be_or_md_finite_layer(Code,L,L0) -->
  modal(Code,L,L1),
  optional_clitic_negation(L1,L0).

% Verb sequencing

ip_ppl_active(Ext,Store,SbjType,Infl,[node(Label,VL)|L],L) -->
  {
    atom_concat('IP-PPL',Ext,Label)
  },
  verb_phrase_layer(Store,SbjType,Infl,active,VL,[]).

% Verb sequencing with passive

ip_ppl_passive(Ext,Store,SbjType,Infl,[node(Label,[node('NP-LGS',[node('*',[])])|VL])|L],L) -->
  {
    atom_concat('IP-PPL',Ext,Label)
  },
  verb_phrase_layer(Store,SbjType,Infl,passive,VL,[]).

% Clause middle layer

clause_middle_layer(Store,SbjType,L1,L0) -->
  verb_phrase_layer(Store,SbjType,finite,active,L1,L0).
clause_middle_layer(Store,SbjType,L,L0) -->
  do_operator_layer(L,L1),
  verb_phrase_layer(Store,SbjType,do_supported_infinitive,active,L1,L0).
clause_middle_layer(Store,SbjType,L,L0) -->
  have_be_or_md_finite_layer(Code,L,L1),
  verb_complements_top_layer(Code,Store,SbjType,active,L1,L0).
clause_middle_layer(Store,SbjType,L,L0) -->
  adverb_phrase('-NIM',non_privileged,L,L1),
  clause_middle_layer(Store,SbjType,L1,L0).
clause_middle_layer(Store,SbjType,[node('ILYR',[node('ILYR',IL)|CL])|L],L) -->
  clause_middle_layer(Store,SbjType,IL,[]),
  clause_middle_tail(Store,SbjType,CL,[]).

clause_middle_tail(Store,SbjType,[node('CONJP',[CONJ,node('ILYR',IL)])|L],L) -->
  conj(CONJ),
  clause_middle_layer(Store,SbjType,IL,[]).
clause_middle_tail(Store,SbjType,[PU,node('CONJP',[node('ILYR',IL)])|L],L0) -->
  punc(non_final,[PU],[]),
  clause_middle_layer(Store,SbjType,IL,[]),
  clause_middle_tail(Store,SbjType,L,L0).

% CP-THT (that-clause)

cp_that(Ext,Type,Store,[node(Label,[node('IP-SUB',IL)])|L],L) -->
  {
    atom_concat('CP-THT',Ext,Label)
  },
  clause_that_layer(Type,Store,IL,[]).
cp_that(Ext,Type,Store,[node(Label,[node('IP-SUB',[node('ILYR',[node('ILYR',IL)|CL])])])|L],L) -->
  {
    atom_concat('CP-THT',Ext,Label)
  },
  clause_that_layer(Type,Store,IL,[]),
  cp_that_tail(Type,Store,CL,[]).

cp_that_tail(Type,Store,[node('CONJP',[CONJ,node('ILYR',IL)])|L],L) -->
  conj(CONJ),
  clause_that_layer(Type,Store,IL,[]).
cp_that_tail(Type,Store,[PU,node('CONJP',[node('ILYR',IL)])|L],L0) -->
  punc(non_final,[PU],[]),
  clause_that_layer(Type,Store,IL,[]),
  cp_that_tail(Type,Store,L,L0).

clause_that_layer(without_c,[np(ICH)|Store],[node('NP-SBJ',[ICH])|L],L0) -->
  clause_middle_layer(Store,filled_sbj,L,L0).
clause_that_layer(with_c,Store,L,L0) -->
  comp(L,L1),
  clause_top_layer(statement_order,Store,L1,L0).
clause_that_layer(without_c,Store,L,L0) -->
  clause_top_layer(statement_order,Store,L,L0).

% TO infinitive layer

to_inf_layer(Store,SbjType,Voice,L,L0) -->
  {
    Voice == active
  },
  to(L,L1),
  verb_phrase_layer(Store,SbjType,infinitive,Voice,L1,L0).
to_inf_layer(Store,SbjType,Voice,[node('NP-LGS',[node('*',[])])|L],L0) -->
  {
    Voice == passive
  },
  to(L,L1),
  verb_phrase_layer(Store,SbjType,infinitive,Voice,L1,L0).
to_inf_layer(Store,SbjType,Voice,L,L0) -->
  {
    Voice == lgs_passive
  },
  to(L,L1),
  verb_phrase_layer(Store,SbjType,infinitive,passive,L1,L0).
to_inf_layer(Store,SbjType0,Voice,L,L0) -->
  {
    SbjType0 == unfilled_sbj,
    Voice == active
  },
  conn(L,L2),
  subject(SbjType,L2,L1),
  to_inf_layer(Store,SbjType,Voice,L1,L0).
to_inf_layer(Store,SbjType0,Voice,L,L0) -->
  {
    SbjType0 == unfilled_sbj,
    Voice == passive
  },
  conn(L,L2),
  noun_phrase('-LGS',non_privileged,L2,L1),
  to_inf_layer(Store,filled_sbj,lgs_passive,L1,L0).
to_inf_layer(Store,SbjType,Voice,[node('ILYR',[node('ILYR',IL)|CL])|L],L) -->
  to_inf_layer(Store,SbjType,Voice,IL,[]),
  to_inf_tail(Store,SbjType,Voice,CL,[]).

to_inf_tail(Store,SbjType,Voice,[node('CONJP',[CONJ,node('ILYR',IL)])|L],L) -->
  conj(CONJ),
  to_inf_layer(Store,SbjType,Voice,IL,[]).
to_inf_tail(Store,SbjType,Voice,[PU,node('CONJP',[node('ILYR',IL)])|L],L0) -->
  punc(non_final,[PU],[]),
  to_inf_layer(Store,SbjType,Voice,IL,[]),
  to_inf_tail(Store,SbjType,Voice,L,L0).

% CP-QUE (embedded question clause)

cp_embedded_que(Ext,Store,[node(Label,[node('IP-SUB',IL)])|L],L) -->
  {
    atom_concat('CP-QUE',Ext,Label)
  },
  clause_embedded_que_finite_top_layer(Store,IL,[]).
cp_embedded_que(Ext,Store,[node(Label,[node('IP-SUB',[node('ILYR',[node('ILYR',IL)|CL])])])|L],L) -->
  {
    atom_concat('CP-QUE',Ext,Label)
  },
  clause_embedded_que_finite_top_layer(Store,IL,[]),
  cp_embedded_que_finite_tail(Store,CL,[]).
cp_embedded_que(Ext,Store,[node(Label,[node('IP-INF',IL)])|L],L) -->
  {
    atom_concat('CP-QUE',Ext,Label)
  },
  clause_embedded_que_to_inf_top_layer(Store,IL,[]).
cp_embedded_que(Ext,Store,[node(Label,[node('IP-INF',[node('ILYR',[node('ILYR',IL)|CL])])])|L],L) -->
  {
    atom_concat('CP-QUE',Ext,Label)
  },
  clause_embedded_que_to_inf_top_layer(Store,IL,[]),
  cp_embedded_que_to_inf_tail(Store,CL,[]).

cp_embedded_que_finite_tail(Store,[node('CONJP',[CONJ,node('ILYR',IL)])|L],L) -->
  conj(CONJ),
  clause_embedded_que_finite_top_layer(Store,IL,[]).
cp_embedded_que_finite_tail(Store,[PU,node('CONJP',[node('ILYR',IL)])|L],L0) -->
  punc(non_final,[PU],[]),
  clause_embedded_que_finite_top_layer(Store,IL,[]),
  cp_embedded_que_finite_tail(Store,L,L0).

cp_embedded_que_to_inf_tail(Store,[node('CONJP',[CONJ,node('ILYR',IL)])|L],L) -->
  conj(CONJ),
  clause_embedded_que_to_inf_top_layer(Store,IL,[]).
cp_embedded_que_to_inf_tail(Store,[PU,node('CONJP',[node('ILYR',IL)])|L],L0) -->
  punc(non_final,[PU],[]),
  clause_embedded_que_to_inf_top_layer(Store,IL,[]),
  cp_embedded_que_to_inf_tail(Store,L,L0).

clause_embedded_que_finite_top_layer(Store,L,L0) -->
  comp_wq(L,L1),
  clause_top_layer(statement_order,Store,L1,L0).
clause_embedded_que_finite_top_layer(Store,L,L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  noun_phrase(Index,interrogative,L,L1),
  clause_embedded_que_finite_lower_layer([np(node(ICH,[]))|Store],L1,L0).
clause_embedded_que_finite_top_layer(Store,L,L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  adverb_phrase(Index,interrogative,L,L1),
  clause_top_layer(statement_order,[advp(node(ICH,[]))|Store],L1,L0).
clause_embedded_que_finite_top_layer(Store,L,L0) -->
  adverb_phrase('-NIM',interrogative,L,L1),
  clause_top_layer(statement_order,Store,L1,L0).

clause_embedded_que_finite_lower_layer([np(ICH)|Store],[node('NP-SBJ',[ICH])|L],L0) -->
  clause_middle_layer(Store,filled_sbj,L,L0).
clause_embedded_que_finite_lower_layer(Store,L,L0) -->
  clause_top_layer(statement_order,Store,L,L0).

clause_embedded_que_to_inf_top_layer(Store,L,L0) -->
  comp_wq(L,L1),
  to_inf_layer(Store,filled_sbj,active,L1,L0).
clause_embedded_que_to_inf_top_layer(Store,L,L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  noun_phrase(Index,interrogative,L,L1),
  to_inf_layer([np(node(ICH,[]))|Store],filled_sbj,active,L1,L0).
clause_embedded_que_to_inf_top_layer(Store,L,L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  adverb_phrase(Index,interrogative,L,L1),
  to_inf_layer([advp(node(ICH,[]))|Store],filled_sbj,active,L1,L0).
clause_embedded_que_to_inf_top_layer(Store,L,L0) -->
  adverb_phrase('-NIM',interrogative,L,L1),
  to_inf_layer(Store,filled_sbj,active,L1,L0).

% notional item

notional_item(Ext,L,L0) -->
  cp_that(Ext,_,[],L,L0).
notional_item(Ext,L,L0) -->
  cp_embedded_que(Ext,[],L,L0).
notional_item(Ext,L,L0) -->
  ip_to_inf(Ext,[],L,L0).
notional_item(Ext,L,L0) -->
  ip_ppl_active(Ext,[],filled_sbj,ing_participle,L,L0).

ip_to_inf(Ext,Store,[node(Label,VL)|L],L) -->
  {
    atom_concat('IP-INF',Ext,Label)
  },
  to_inf_top_layer(Store,VL,[]).

to_inf_top_layer(Store,L,L0) -->
  to_inf_layer(Store,unfilled_sbj,active,L,L0).
to_inf_top_layer(Store,L,L0) -->
  to_inf_layer(Store,filled_sbj,active,L,L0).

% subordinate conjunction clause

scon_clause([node('PP-SCON',PL)|L],L) -->
  conn(PL,[node('IP-ADV',IL)]),
  clause_top_layer(statement_order,[],IL,[]).
scon_clause([node('PP-SCON',PL)|L],L) -->
  conn(PL,[node('IP-INF2;IP-INF',VL)]),
  to_inf_layer([],filled_sbj,active,VL,[]).
scon_clause([node('PP-SCON',PL)|L],L) -->
  conn(PL,[node('IP-PPL2;IP-PPL',IL)]),
  {
    member(Infl,[hag_participle,en_participle,ing_participle])
  },
  ip_ppl_adverbial_layer(filled_sbj,Infl,IL,[]).
scon_clause([node('PP-SCON',PL)|L],L) -->
  conn(PL,[node('IP-PPL3',IL)]),
  {
    member(Infl,[hag_participle,en_participle,ing_participle])
  },
  ip_ppl_adverbial_layer(unfilled_sbj,Infl,IL,[]).
scon_clause([node('PP',[PP1|CL])|L],L) -->
  scon_clause([PP1],[]),
  scon_clause_tail(CL,[]).

scon_clause_tail([node('CONJP',[CONJ,PP])|L],L) -->
  conj(CONJ),
  scon_clause([PP],[]).
scon_clause_tail([PU,node('CONJP',[PP])|L],L0) -->
  punc(non_final,[PU],[]),
  scon_clause([PP],[]),
  scon_clause_tail(L,L0).

ip_ppl_adverbial_layer(SbjType0,Infl,L,L0) -->
  {
    SbjType0 == unfilled_sbj
  },
  subject(SbjType,L,L1),
  ip_ppl_adverbial_layer(SbjType,Infl,L1,L0).
ip_ppl_adverbial_layer(SbjType,Infl,[node('HAG;~cat_Ve',[node(Word,[])])|L],L0) -->
  {
    Infl == hag_participle
  },
  [w('HAG',';~cat_Ve',Word)],
  ip_ppl_active('-CAT',[],SbjType,en_participle,L,L0).
ip_ppl_adverbial_layer(SbjType,Infl,L,L0) -->
  {
    Infl == ing_participle
  },
  verb_phrase_layer([],SbjType,Infl,active,L,L0).
ip_ppl_adverbial_layer(SbjType,Infl,L,L0) -->
  {
    Infl == en_participle
  },
  verb_phrase_layer([],SbjType,Infl,active,L,L0).
ip_ppl_adverbial_layer(SbjType,Infl,[node('NP-LGS',[node('*',[])])|L],L0) -->
  {
    Infl == en_participle
  },
  verb_phrase_layer([],SbjType,Infl,passive,L,L0).
ip_ppl_adverbial_layer(SbjType,Infl,[node('ILYR',[node('ILYR',IL)|CL])|L],L) -->
  ip_ppl_adverbial_layer(SbjType,Infl,IL,[]),
  ip_ppl_adverbial_tail(SbjType,Infl,CL,[]).

ip_ppl_adverbial_tail(SbjType,Infl,[node('CONJP',[CONJ,node('ILYR',IL)])|L],L) -->
  conj(CONJ),
  ip_ppl_adverbial_layer(SbjType,Infl,IL,[]).
ip_ppl_adverbial_tail(SbjType,Infl,[PU,node('CONJP',[node('ILYR',IL)])|L],L0) -->
  punc(non_final,[PU],[]),
  ip_ppl_adverbial_layer(SbjType,Infl,IL,[]),
  ip_ppl_adverbial_tail(SbjType,Infl,L,L0).

% Relative clauses

relative_clause([node('IP-REL',IL)|L],L) -->
  relative_clause_finite_top_layer(IL,[]).
relative_clause([node('IP-INF-REL',IL)|L],L) -->
  relative_clause_to_inf_top_layer(IL,[]).
relative_clause(L,L0) -->
  ip_ppl_active('',[],filled_sbj,ing_participle,L,L0).
relative_clause(L,L0) -->
  ip_ppl_passive('',[],filled_sbj,en_participle,L,L0).

relative_clause_finite_top_layer(L,L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  noun_phrase(Index,relative,L,L1),
  relative_clause_finite_inside([np(node(ICH,[]))],L1,L0).
relative_clause_finite_top_layer(L,L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  preposition_phrase(Index,relative,L,L1),
  clause_top_layer(statement_order,[pp(node(ICH,[]))],L1,L0).
relative_clause_finite_top_layer(L,L0) -->
  preposition_phrase('-NIM',relative,L,L1),
  clause_top_layer(statement_order,[],L1,L0).
relative_clause_finite_top_layer(L,L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  adverb_phrase(Index,relative,L,L1),
  clause_top_layer(statement_order,[advp(node(ICH,[]))],L1,L0).
relative_clause_finite_top_layer(L,L0) -->
  adverb_phrase('-NIM',relative,L,L1),
  clause_top_layer(statement_order,[],L1,L0).
relative_clause_finite_top_layer(L,L0) -->
  clause_top_layer(statement_order,[np(node('*T*',[]))],L,L0).
relative_clause_finite_top_layer([node('NP-NIM',[node('*T*',[])])|L],L0) -->
  clause_top_layer(statement_order,[],L,L0).
relative_clause_finite_top_layer([node('ILYR',[node('ILYR',IL)|CL])|L],L) -->
  relative_clause_finite_top_layer(IL,[]),
  relative_clause_finite_top_tail(CL,[]).

relative_clause_finite_top_tail([node('CONJP',[CONJ,node('ILYR',IL)])|L],L) -->
  conj(CONJ),
  relative_clause_finite_top_layer(IL,[]).
relative_clause_finite_top_tail([PU,node('CONJP',[node('ILYR',IL)])|L],L0) -->
  punc(non_final,[PU],[]),
  relative_clause_finite_top_layer(IL,[]),
  relative_clause_finite_top_tail(L,L0).

relative_clause_finite_inside([np(ICH)],[node('NP-SBJ',[ICH])|L],L0) -->
  clause_middle_layer([],filled_sbj,L,L0).
relative_clause_finite_inside(Store,L,L0) -->
  clause_top_layer(statement_order,Store,L,L0).

relative_clause_to_inf_top_layer(L,L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  preposition_phrase(Index,relative,L,L1),
  to_inf_top_layer([pp(node(ICH,[]))],L1,L0).
relative_clause_to_inf_top_layer(L,L0) -->
  preposition_phrase('-NIM',relative,L,L1),
  to_inf_top_layer([],L1,L0).
relative_clause_to_inf_top_layer(L,L0) -->
  to_inf_top_layer([np(node('*T*',[]))],L,L0).
relative_clause_to_inf_top_layer([node('NP-SBJ',[node('*T*',[])])|L],L0) -->
  to_inf_layer([],filled_sbj,active,L,L0).
relative_clause_to_inf_top_layer([node('NP-NIM',[node('*T*',[])])|L],L0) -->
  to_inf_top_layer([],L,L0).
relative_clause_to_inf_top_layer([node('ILYR',[node('ILYR',IL)|CL])|L],L) -->
  relative_clause_to_inf_top_layer(IL,[]),
  relative_clause_to_inf_top_tail(CL,[]).

relative_clause_to_inf_top_tail([node('CONJP',[CONJ,node('ILYR',IL)])|L],L) -->
  conj(CONJ),
  relative_clause_to_inf_top_layer(IL,[]).
relative_clause_to_inf_top_tail([PU,node('CONJP',[node('ILYR',IL)])|L],L0) -->
  punc(non_final,[PU],[]),
  relative_clause_to_inf_top_layer(IL,[]),
  relative_clause_to_inf_top_tail(L,L0).

