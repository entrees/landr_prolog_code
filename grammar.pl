% Example queries:
% | ?- sentence([Tree],store([],[]),[w('PRO','He'),w('VBP',';~I','smiles'),w('CONJ','and'),w('VBP',';~I','smiles'),w('PUNC','.')],[]).
% | ?- tphrase_set_string([w('PRO','He'),w('VBP',';~I','smiles'),w('PUNC','.')]), tphrase(sentence([Tree],[])).

:- import member/2 from basics.
:- import append/3 from basics.
:- import gensym/2 from gensym.

:- auto_table.

% Sentence

sentence([node('IP-MAT',IL)|L],L) -->
  clause_top_layer(statement_order,store([],[]),IL,IL1),
  punc(final,IL1,[]).
sentence([node('IP-IMP',IL)|L],L) -->
  clause_top_layer(imperative_clause,store([],[]),IL,IL1),
  punc(final,IL1,[]).
sentence([node('CP-QUE-MAT',[node('IP-SUB',IL),PU])|L],L) -->
  clause_top_layer(matrix_interrogative,store([],[]),IL,[]),
  punc(final_question,[PU],[]).
sentence([node('IP-MAT',IL)|L],L) -->
  clause_top_layer(statement_order,store([],[]),IL,IL2),
  punc(non_final,IL2,[node('CP-QUE-TAG',[node('IP-SUB',TL)])|IL1]),
  clause_top_layer(tag_question,store([],[]),TL,[]),
  punc(final_question,IL1,[]).
sentence([node('IP-MAT',[node('ILYR',[node('ILYR',IL1),node('CONJP',[CONJ,node('ILYR',[node('ADVP-CLR',[node('ADV',[node('so',[])])])|IL2])])]),PU])|L],L) -->
  clause_top_layer(statement_order,store([],[]),IL1,[]),
  conj(CONJ),
  [w('ADV','so')],
  clause_top_layer(tag_question,store([],[]),IL2,[]),
  punc(final,[PU],[]).

sentence([node('IP-MAT',IL)|L],L) -->
  {
    gensym('-',Index),
    atom_concat('-RNR',Index,Extra),
    atom_concat('*ICH*',Index,ICH)
  },
  clause_top_layer(statement_order,store([],[ICH_item]),IL,IL3),
  punc(non_final,IL3,IL2),
  displaced_item(ICH,ICH_item,Extra,IL2,IL1),
  punc(final,IL1,[]).

displaced_item(ICH,np(node(ICH,[])),Ext,L,L0) -->
  noun_phrase(Ext,established,L,L0).
displaced_item(ICH,pp(node(ICH,[])),Ext,L,L0) -->
  preposition_phrase(Ext,established,L,L0).
displaced_item(ICH,pp_lgs(node(ICH,[])),Ext,[node(Label,[node('P-ROLE;_lgs_',[node(Word,[])]),NP])|L],L) -->
  {
    atom_concat('PP',Ext,Label)
  },
  [w('P-ROLE;_lgs_',Word)],
  noun_phrase('',established,[NP],[]).
displaced_item(ICH,cp_that(node(ICH,[])),Ext,L,L0) -->
  cp_that(Ext,_,store([],[]),L,L0).
displaced_item(ICH,cp_embedded_que(node(ICH,[])),Ext,L,L0) -->
  cp_embedded_que(Ext,store([],[]),L,L0).
displaced_item(ICH,ip_to_inf(node(ICH,[])),Ext,L,L0) -->
  ip_to_inf(Ext,store([],[]),L,L0).
displaced_item(ICH,ip_ppl(node(ICH,[])),Ext,L,L0) -->
  ip_ppl_active(Ext,store([],[]),filled_sbj,ing_participle,L,L0).
displaced_item(ICH,utterance(node(ICH,[])),Ext,L,L0) -->
  punc(left_quotation_mark,L,L2),
  utterance(Ext,L2,L1),
  punc(right_quotation_mark,L1,L0).

% Fragment

fragment([node('FRAG',FL)|L],L) -->
  fragment_layer(FL,FL1),
  punc(final,FL1,[]).

fragment_layer(L,L0) -->
  noun_phrase('',_,L,L0).
fragment_layer(L,L0) -->
  {
    member(Type,[established,interrogative,relative,filled_sbj])
  },
  adjective_phrase('',store([],[]),Type,L,L0).
fragment_layer(L,L0) -->
  initial_adverbial(L,L0).
fragment_layer(L,L0) -->
  adverbial(L,L0).
fragment_layer(L,L0) -->
  ip_to_inf('',store([],[]),L,L0).
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
    member(Type,[non_privileged,established])
  },
  [w('PRO',Word)].
noun_head_full(Type,[node('PRO;_expletive_',[node(Word,[])])|L],L) -->
  {
    member(Type,[non_privileged,established])
  },
  [w('PRO;_expletive_',Word)].
noun_head_full(Type,[node('NP-GEN',[node('PRO;_ppge_',[node(Word,[])])])|L],L) -->
  {
    member(Type,[non_privileged,established])
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
    member(Type,[non_privileged,established])
  },
  [w('Q',Word)].
det(Type,[node('D',[node(Word,[])])|L],L) -->
  {
    member(Type,[non_privileged,established])
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
    member(Type,[non_privileged,established])
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
subject(provisional_sbj,[node('NP-SBJ',[node('PRO;_provisional_',[node(Word,[])])])|L],L) -->
  [w('PRO;_provisional_',Word)].
subject(filled_sbj,L,L0) -->
  noun_phrase('-SBJ',established,L,L0).
subject(derived_sbj,L,L0) -->
  noun_phrase('-SBJ',established,L,L0).
subject(filled_sbj,L,L0) -->
  ip_ppl_active('-SBJ',store([],[]),filled_sbj,ing_participle,L,L0).

% Adverbs

adv(established,[node('ADV',[node(Word,[])])|L],L) -->
  [w('ADV',Word)].
adv(established,[node('ADVR',[node(Word,[])])|L],L) -->
  [w('ADVR',Word)].
adv(established,[node('ADVS',[node(Word,[])])|L],L) -->
  [w('ADVS',Word)].
adv(interrogative,[node('WADV',[node(Word,[])])|L],L) -->
  [w('WADV',Word)].
adv(relative,[node('RADV',[node(Word,[])])|L],L) -->
  [w('RADV',Word)].
adv(particle,[node('RP',[node(Word,[])])|L],L) -->
  [w('RP',Word)].

% Adjectives

adj(established,[node('ADJ',[node(Word,[])])|L],L) -->
  [w('ADJ',Word)].
adj(established,[node('ADJR',[node(Word,[])])|L],L) -->
  [w('ADJR',Word)].
adj(established,[node('ADJS',[node(Word,[])])|L],L) -->
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
     ';~Tt',';~Tnt',';~Tprt',';~Tni',';~Tg',';~Tng',';~Tsg',
     ';~Dn.n',';~Dn.f',';~Dn.w',';~Dn.r',';~Dn.t',';~Dn.*',
     ';~Dn.pr',';~Dpr.f',';~Dpr.r',
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
     ';~I',';~Ip',';~Ipr',
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
  adverb_phrase('-NIM',established,L,L0).
initial_adverbial(L,L0) -->
  preposition_phrase('-NIM-TPC',established,L,L0).
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
punc(final,[node('PUNC;_final_',[node(Word,[])])|L],L) -->
  [w('PUNC;_final_',Word)].
punc(final_question,[node('PUNC;_final_question_',[node(Word,[])])|L],L) -->
  [w('PUNC;_final_question_',Word)].
punc(non_final,[node('PUNC;_non_final_',[node(Word,[])])|L],L) -->
  [w('PUNC;_non_final_',Word)].

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
    member(Type,[non_privileged,interrogative])
  },
  adjective_phrase('',store([],[]),interrogative,L,L1),
  internal_np_higher_layer(L1,L0).
noun_phrase_initial_layer(Type,L,L0) -->
  {
    member(Type,[non_privileged,established])
  },
  noun_head(L,L0).
noun_phrase_initial_layer(Type,L,L0) -->
  {
    member(Type,[non_privileged,established])
  },
  internal_np_higher_layer(L,L0).
noun_phrase_initial_layer(Type,[node('NLYR',[CONJ1,node('NLYR',NL1),node('CONJP',[CONJ2,node('NLYR',NL2)])])|L],L) -->
  conj(CONJ1),
  noun_phrase_top(Type,NL1,[]),
  conj(CONJ2),
  noun_phrase_top(Type,NL2,[]).
noun_phrase_initial_layer(Type,[node('NLYR',[node('NLYR',NL)|CL])|L],L) -->
  noun_phrase_top(Type,NL,[]),
  noun_phrase_initial_tail(Type,CL,[]).
noun_phrase_initial_layer(Type,L,L0) -->
  noun_phrase_initial_layer(Type,L,L1),
  preposition_phrase('',established,L1,L0).
noun_phrase_initial_layer(Type,L,L0) -->
  {
    Type == relative
  },
  noun_phrase_initial_layer(established,L,L1),
  preposition_phrase('',relative,L1,L0).
noun_phrase_initial_layer(Type,L,L0) -->
  noun_phrase_initial_layer(Type,L,L1),
  relative_clause(L1,L0).

noun_phrase_initial_tail(Type,[node('CONJP',[CONJ,node('NLYR',NL)])|L],L) -->
  conj(CONJ),
  noun_phrase_top(Type,NL,[]).
noun_phrase_initial_tail(Type,[PU,node('CONJP',[node('NLYR',NL)])|L],L0) -->
  punc(non_final,[PU],[]),
  noun_phrase_top(Type,NL,[]),
  noun_phrase_initial_tail(Type,L,L0).

internal_np_higher_layer(L,L0) -->
  internal_np_lower_layer(_,L,L0).

internal_np_higher_layer(L,L0) -->
  adjective_phrase('',store([],[]),established,L,L1),
  internal_np_higher_layer(L1,L0).
internal_np_higher_layer([node('IP-PPL',[node('VAG;~I',[node(Word,[])])])|L],L0) -->
  [w('VAG',';~I',Word)],
  internal_np_higher_layer(L,L0).
internal_np_higher_layer([node('IP-PPL',[node('NP-LGS',[node('*',[])]),node('VVN;~Tn',[node(Word,[])])])|L],L0) -->
  [w('VVN',';~Tn',Word)],
  internal_np_higher_layer(L,L0).
internal_np_higher_layer(L,L0) -->
  internal_np_higher_layer(L,L1),
  preposition_phrase('',established,L1,L0).
internal_np_higher_layer(L,L0) -->
  internal_np_higher_layer(L,L1),
  relative_clause(L1,L0).
internal_np_higher_layer([node('NLYR',[node('NLYR',NL)|CL])|L],L) -->
  internal_np_higher_layer(NL,[]),
  internal_np_higher_tail(CL,[]).

internal_np_higher_tail([node('CONJP',[CONJ,node('NLYR',NL)])|L],L) -->
  conj(CONJ),
  internal_np_higher_layer(NL,[]).
internal_np_higher_tail([PU,node('CONJP',[node('NLYR',NL)])|L],L0) -->
  punc(non_final,[PU],[]),
  internal_np_higher_layer(NL,[]),
  internal_np_higher_tail(L,L0).

internal_np_lower_layer(simple,L,L0) -->
  noun(L,L0).
internal_np_lower_layer(complex,L,L0) -->
  noun(L,L1),
  ip_to_inf('',store([],[]),L1,L0).
internal_np_lower_layer(complex,L,L0) -->
  noun(L,L1),
  cp_that('',with_c,store([],[]),L1,L0).
internal_np_lower_layer(complex,L,L0) -->
  noun(L,L1),
  cp_embedded_que('',store([],[]),L1,L0).

internal_np_lower_layer(Type,L,L0) -->
  noun(L,L1),
  internal_np_lower_layer(Type,L1,L0).
internal_np_lower_layer(Type,[node('NLYR',[NL1,NL2|NL3])|L],L0) -->
  internal_np_lower_layer(simple,[NL1,NL2|NL3],[]),
  internal_np_lower_layer(Type,L,L0).
internal_np_lower_layer(Type,[node('NLYR',[node('NLYR',[node('NLYR',NL)|CL])])|L],L0) -->
  internal_np_lower_layer(simple,NL,[]),
  internal_np_lower_tail(simple,CL,[]),
  internal_np_lower_layer(Type,L,L0).
internal_np_lower_layer(Type,[node('NLYR',[node('NLYR',NL)|CL])|L],L) -->
  internal_np_lower_layer(Type,NL,[]),
  internal_np_lower_tail(Type,CL,[]).

internal_np_lower_tail(Type,[node('CONJP',[CONJ,node('NLYR',NL)])|L],L) -->
  conj(CONJ),
  internal_np_lower_layer(Type,NL,[]).
internal_np_lower_tail([PU,node('CONJP',[node('NLYR',NL)])|L],L0) -->
  punc(non_final,[PU],[]),
  internal_np_lower_layer(Type,NL,[]),
  internal_np_lower_tail(Type,L,L0).

% Determiner layer

determiner_layer(Type,L,L0) -->
  det(Type,L,L0).
determiner_layer(Type,[node('NP-GEN',NL)|L],L) -->
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
    member(Type,[established,interrogative,relative,particle]),
    atom_concat('ADVP',Ext,Label)
  },
  adverb_phrase_layer(Type,AL,[]).
adverb_phrase(Ext,Type1,[node(Label,AL)|L],L) -->
  {
    Type1 == non_privileged,
    member(Type2,[established,interrogative]),
    atom_concat('ADVP',Ext,Label)
  },
  adverb_phrase_layer(Type2,AL,[]).

adverb_phrase_layer(Type,L,L0) -->
  adv(Type,L,L0).
adverb_phrase_layer(established,L,L0) -->
  noun_phrase('',established,L,L1),
  adverb_phrase_layer(established,L1,L0).
adverb_phrase_layer(established,L,L0) -->
  neg(L,L1),
  adverb_phrase_layer(established,L1,L0).
adverb_phrase_layer(Type,L,L0) -->
  adverb_phrase('',Type,L,L1),
  adverb_phrase_layer(established,L1,L0).
adverb_phrase_layer(established,L,L0) -->
  adverb_phrase_layer(established,L,L1),
  preposition_phrase('',established,L1,L0).
adverb_phrase_layer(Type,[node('AVLYR',[node('AVLYR',AL)|CL])|L],L) -->
  adverb_phrase_layer(Type,AL,[]),
  adverb_phrase_tail(Type,CL,[]).

adverb_phrase_tail(Type,[node('CONJP',[CONJ,node('AVLYR',AL)])|L],L) -->
  conj(CONJ),
  adverb_phrase_layer(Type,AL,[]).
adverb_phrase_tail(Type,[PU,node('CONJP',[node('AVLYR',AL)])|L],L0) -->
  punc(non_final,[PU],[]),
  adverb_phrase_layer(Type,AL,[]),
  adverb_phrase_tail(Type,L,L0).

% Adjective phrase

adjective_phrase(Ext,Store,Type,[node(Label,AL)|L],L) -->
  {
    member(Type,[established,interrogative,relative,filled_sbj,there_sbj,provisional_sbj,derived_sbj]),
    atom_concat('ADJP',Ext,Label)
  },
  adjective_phrase_layer(Store,Type,AL,[]).
adjective_phrase(Ext,store([],[]),Type1,[node(Label,AL)|L],L) -->
  {
    Type1 == non_privileged,
    member(Type2,[established,interrogative]),
    atom_concat('ADJP',Ext,Label)
  },
  adjective_phrase_layer(store([],[]),Type2,AL,[]).

adjective_phrase_layer(store([],[]),Type,L,L0) -->
  {
    member(Type,[interrogative,relative])
  },
  adverb_phrase('',Type,L,L1),
  adjective_phrase_layer(store([],[]),established,L1,L0).
adjective_phrase_layer(Store,SbjType,L,L0) -->
  adj(catenative,L,[node('IP-INF',VL)|L0]),
  to_inf_layer(Store,SbjType,active,VL,[]).
adjective_phrase_layer(store([],[]),established,L,L0) -->
  adj(established,L,L0).
adjective_phrase_layer(store([],[]),established,L,L0) -->
  adj(established,L,L1),
  cp_that('',with_c,store([],[]),L1,L0).
adjective_phrase_layer(store([],[]),established,L,L0) -->
  noun_phrase('',established,L,L1),
  adjective_phrase_layer(store([],[]),established,L1,L0).
adjective_phrase_layer(store([],[]),established,L,L0) -->
  neg(L,L1),
  adjective_phrase_layer(store([],[]),established,L1,L0).
adjective_phrase_layer(store([],[]),established,L,L0) -->
  adverb_phrase('',established,L,L1),
  adjective_phrase_layer(store([],[]),established,L1,L0).
adjective_phrase_layer(store([],[]),established,L,L0) -->
  adjective_phrase_layer(store([],[]),established,L,L1),
  preposition_phrase('',established,L1,L0).
adjective_phrase_layer(Store,SbjType,L,L0) -->
  adj(established,L,[node('PP',PL)|L0]),
  role(PL,VL),
  ip_ppl_active('',Store,SbjType,ing_participle,VL,[]).
adjective_phrase_layer(Store,Type,[node('AJLYR',[node('AJLYR',AL)|CL])|L],L) -->
  adjective_phrase_layer(Store,Type,AL,[]),
  adjective_phrase_tail(Store,Type,CL,[]).

adjective_phrase_tail(Store,Type,[node('CONJP',[CONJ,node('AJLYR',AL)])|L],L) -->
  conj(CONJ),
  adjective_phrase_layer(Store,Type,AL,[]).
adjective_phrase_tail(Store,Type,[PU,node('CONJP',[node('AJLYR',AL)])|L],L0) -->
  punc(non_final,[PU],[]),
  adjective_phrase_layer(Store,Type,AL,[]),
  adjective_phrase_tail(Store,Type,L,L0).

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
    member(Type,[non_privileged,established])
  },
  role(L,[node('IP-PPL',IL)|L0]),
  ip_ppl_adverbial_layer(filled_sbj,ing_participle,IL,[]).
preposition_phrase_layer(Type,L,L0) -->
  {
    member(Type,[non_privileged,established])
  },
  role(L,[node('IP-PPL3',IL)|L0]),
  ip_ppl_adverbial_layer(unfilled_sbj,ing_participle,IL,[]).
preposition_phrase_layer(Type,L,L0) -->
  {
    member(Type,[non_privileged,established])
  },
  role(L,L1),
  cp_embedded_que('',store([],[]),L1,L0).
preposition_phrase_layer(Type,L,L0) -->
  {
    member(Type,[non_privileged,established])
  },
  role(L,[node('IP-ADV',IL)|L0]),
  clause_top_layer(statement_order,store([],[]),IL,[]).

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

verb_complements_top_layer(Code,Store,SbjType0,Voice,L,L0) -->
  {
    SbjType0 == provisional_sbj,
    member(SbjType,[filled_sbj,derived_sbj])
  },
  verb_complements_top_layer(Code,store([],[]),SbjType,Voice,L,L1),
  notional_item('-NSBJ',Store,L1,L0).
verb_complements_top_layer(Code,Store,SbjType0,Voice,L,L0) -->
  {
    SbjType0 == provisional_sbj,
    member(SbjType,[filled_sbj,derived_sbj])
  },
  verb_complements_top_layer(Code,Store,SbjType,Voice,L,L1),
  notional_item('-NSBJ',store([],[]),L1,L0).

verb_complements_top_layer(Code,Store,SbjType,Voice,L,L0) -->
  {
    Code == ';~cat_Ve_passive_',
    SbjType == derived_sbj
  },
  verb_complements_top_layer(Code,store([],[]),filled_sbj,Voice,L,[node('IP-INF-NSBJ',VL)|L0]),
  to_inf_layer(Store,filled_sbj,active,VL,[]).

verb_complements_top_layer(Code,Store,SbjType,Voice,L,L0) -->
  {
    member(Code,[';~La',';~Tn']),
    SbjType == derived_sbj,
    Voice == active,
    member(SbjType1,[filled_sbj,unfilled_sbj])
  },
  verb_complements_top_layer(Code,store([],[]),filled_sbj,Voice,L,[node('IP-INF-NSBJ',VL)|L0]),
  to_inf_layer(Store,SbjType1,passive,VL,[]).

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

verb_complements_by_code(';~La',store([],[]),filled_sbj,active,L,L0) -->
  adjective_phrase('-PRD',store([],[]),non_privileged,L,L0).
verb_complements_by_code(';~La',Store,SbjType,active,L,L0) -->
  adjective_phrase('-PRD',Store,SbjType,L,L0).
verb_complements_by_code(';~La',store([adjp(ICH)],[]),filled_sbj,active,[node('ADJP-PRD',[ICH])|L],L) -->
  [].

verb_complements_by_code(';~Ln',store([],[]),filled_sbj,active,L,L0) -->
  noun_phrase('-PRD',non_privileged,L,L0).
verb_complements_by_code(';~Ln',store([np(ICH)],[]),filled_sbj,active,[node('NP-PRD',[ICH])|L],L) -->
  [].

% Intransitive verb complements

verb_complements_by_code(';~I',store([],[]),filled_sbj,active,L,L) -->
  [].

verb_complements_by_code(';~Ip',store([],[]),filled_sbj,active,L,L0) -->
  adverb_phrase('-CLR',particle,L,L0).
verb_complements_by_code(';~Ip',store([advp(ICH)],[]),filled_sbj,active,[node('ADVP-CLR',[ICH])|L],L) -->
  [].

verb_complements_by_code(';~Ipr',store([],[]),filled_sbj,active,L,L0) -->
  preposition_phrase('-CLR',non_privileged,L,L0).
verb_complements_by_code(';~Ipr',store(Left,Right),filled_sbj,active,[node('PP-CLR',[Role,node('NP',[ICH])])|L],L) -->
  {
    append(Left,Right,[np(ICH)])
  },
  role([Role],[]).
verb_complements_by_code(';~Ipr',store(Left,Right),filled_sbj,active,[node('PP-CLR',[ICH])|L],L) -->
  {
    append(Left,Right,[pp(ICH)])
  },
  [].
verb_complements_by_code(';~Ipr',store([],[]),filled_sbj,lgs_passive,[node('PP-CLR',[Role])|L],L) -->
  role([Role],[]).

verb_complements_by_code(';~In/pr',store([],[]),filled_sbj,active,L,L0) -->
  noun_phrase('-CLR',non_privileged,L,L0).
verb_complements_by_code(';~In/pr',store([],[]),filled_sbj,active,L,L0) -->
  preposition_phrase('-CLR',non_privileged,L,L0).

verb_complements_by_code(';~It',store([],[]),filled_sbj,active,L,L0) -->
  ip_to_inf('-CLR',store([],[]),L,L0).

% Mono-transitive verb complements

verb_complements_by_code(';~Tn',store([],[]),filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,L0).
verb_complements_by_code(';~Tn',store(Left,Right),filled_sbj,active,[node('NP-OB1',[ICH])|L],L) -->
  {
    append(Left,Right,[np(ICH)])
  },
  [].
verb_complements_by_code(';~Tn',store([],[]),filled_sbj,lgs_passive,L,L) -->
  [].

verb_complements_by_code(';~Tn.p',store([],[]),filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,L1),
  adverb_phrase('-CLR',particle,L1,L0).
verb_complements_by_code(';~Tn.p',store([],[]),filled_sbj,active,L,L0) -->
  adverb_phrase('-CLR',particle,L,L1),
  noun_phrase('-OB1',non_privileged,L1,L0).
verb_complements_by_code(';~Tn.p',store(Left,Right),filled_sbj,active,[node('NP-OB1',[ICH])|L],L0) -->
  {
    append(Left,Right,[np(ICH)])
  },
  adverb_phrase('-CLR',particle,L,L0).
verb_complements_by_code(';~Tn.p',store(Left,Right),filled_sbj,active,[node('ADVP-CLR',[ICH])|L],L0) -->
  {
    append(Left,Right,[advp(ICH)])
  },
  noun_phrase('-OB1',non_privileged,L,L0).
verb_complements_by_code(';~Tn.p',store(Left,Right),filled_sbj,active,[node('NP-OB1',[ICH1]),node('ADVP-CLR',[ICH2])|L],L) -->
  {
    (
      append(Left,Right,[advp(ICH2),np(ICH1)])
    ;
      append(Left,Right,[np(ICH1),advp(ICH2)])
    )
  },
  [].
verb_complements_by_code(';~Tn.p',store([],[]),filled_sbj,lgs_passive,L,L0) -->
  adverb_phrase('-CLR',particle,L,L0).
verb_complements_by_code(';~Tn.p',store(Left,Right),filled_sbj,lgs_passive,[node('ADVP-CLR',[ICH])|L],L) -->
  {
    append(Left,Right,[advp(ICH)])
  },
  [].

verb_complements_by_code(';~Tn.pr',store([],[]),filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,L1),
  preposition_phrase('-CLR',non_privileged,L1,L0).
verb_complements_by_code(';~Tn.pr',store([],[]),filled_sbj,active,L,L0) -->
  preposition_phrase('-CLR',non_privileged,L,L1),
  noun_phrase('-OB1',non_privileged,L1,L0).
verb_complements_by_code(';~Tn.pr',Store,filled_sbj,active,[node('NP-OB1',[node('PRO;_provisional_',[node(Word,[])])])|L],L0) -->
  [w('PRO;_provisional_',Word)],
  preposition_phrase('-CLR',non_privileged,L,L1),
  notional_item('-NOB1',Store,L1,L0).
verb_complements_by_code(';~Tn.pr',store(Left,Right),filled_sbj,active,L,L0) -->
  {
    append(Left,Right,[np(ICH)])
  },
  noun_phrase('-OB1',non_privileged,L,[node('PP-CLR',[Role,node('NP',[ICH])])|L0]),
  role([Role],[]).
verb_complements_by_code(';~Tn.pr',store(Left,Right),filled_sbj,active,L,L0) -->
  {
    append(Left,Right,[pp(ICH)])
  },
  noun_phrase('-OB1',non_privileged,L,[node('PP-CLR',[ICH])|L0]).
verb_complements_by_code(';~Tn.pr',store(Left,Right),filled_sbj,active,[node('NP-OB1',[ICH])|L],L0) -->
  {
    append(Left,Right,[np(ICH)])
  },
  preposition_phrase('-CLR',non_privileged,L,L0).
verb_complements_by_code(';~Tn.pr',store([],[]),filled_sbj,lgs_passive,L,L0) -->
  preposition_phrase('-CLR',non_privileged,L,L0).
verb_complements_by_code(';~Tn.pr',store(Left,Right),filled_sbj,lgs_passive,[node('PP-CLR',[Role,node('NP',[ICH])])|L],L) -->
  {
    append(Left,Right,[np(ICH)])
  },
  role([Role],[]).
verb_complements_by_code(';~Tn.pr',store(Left,Right),filled_sbj,lgs_passive,[node('PP-CLR',[ICH])|L],L) -->
  {
    append(Left,Right,[pp(ICH)])
  },
  [].
verb_complements_by_code(';~Tn.pr',store([],[]),filled_sbj,lgs_passive,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,[node('PP-CLR',[Role])|L0]),
  role([Role],[]).
verb_complements_by_code(';~Tn.pr',store([],[]),filled_sbj,passive,[node('PP-LGS',[node('P-ROLE;_lgs_',[node(Word,[])]),NP])|L],L0) -->
  [w('P-ROLE;_lgs_',Word)],
  noun_phrase('',non_privileged,[NP],[]),
  preposition_phrase('-CLR',non_privileged,L,L0).

verb_complements_by_code(';~Tf',Store,filled_sbj,active,L,L0) -->
  cp_that('-OB1',_,Store,L,L0).
verb_complements_by_code(';~Tf',store(Left,Right),filled_sbj,active,[node('CP-THT-OB1',[ICH])|L],L) -->
  {
    append(Left,Right,[cp_that(ICH)])
  },
  [].

verb_complements_by_code(';~Tw',Store,filled_sbj,active,L,L0) -->
  cp_embedded_que('-OB1',Store,L,L0).

verb_complements_by_code(';~Tr',store([],[]),filled_sbj,active,L,L0) -->
  optional_punc_non_final(L,L3),
  punc(left_quotation_mark,L3,L2),
  utterance('-OB1',L2,L1),
  punc(right_quotation_mark,L1,L0).
verb_complements_by_code(';~Tr',store(Left,Right),filled_sbj,active,[node('utterance-OB1',[ICH])|L],L) -->
  {
    append(Left,Right,[utterance(ICH)])
  },
  [].

verb_complements_by_code(';~Tt',Store,filled_sbj,active,L,L0) -->
  ip_to_inf('-OB1',Store,L,L0).

verb_complements_by_code(';~Tnt',Store,filled_sbj,active,L,L0) -->
  noun_phrase('-DOB1',non_privileged,L,[node('IP-INF-OB1',VL)|L0]),
  to_inf_layer(Store,filled_sbj,active,VL,[]).
verb_complements_by_code(';~Tnt',store(Left,Right),filled_sbj,active,[node('NP-DOB1',[ICH]),node('IP-INF-OB1',VL)|L],L) -->
  {
    append(Left,Right,[np(ICH)])
  },
  to_inf_layer(store([],[]),filled_sbj,active,VL,[]).
verb_complements_by_code(';~Tnt',Store,filled_sbj,lgs_passive,[node('IP-INF-OB1',VL)|L],L) -->
  to_inf_layer(Store,filled_sbj,active,VL,[]).
verb_complements_by_code(';~Tnt',Store,filled_sbj,passive,[node('PP-LGS',[node('P-ROLE;_lgs_',[node(Word,[])]),NP]),node('IP-INF-OB1',VL)|L],L) -->
  [w('P-ROLE;_lgs_',Word)],
  noun_phrase('',non_privileged,[NP],[]),
  to_inf_layer(Store,filled_sbj,active,VL,[]).
verb_complements_by_code(';~Tnt',Store,filled_sbj,active,[node('NP-DOB1',[node('PRO;_provisional_',[node(Word,[])])]),node('IP-INF-OB1',VL)|L],L0) -->
  [w('PRO;_provisional_',Word)],
  to_inf_layer(store([],[]),filled_sbj,active,VL,[]),
  notional_item('-NDOB1',Store,L,L0).
verb_complements_by_code(';~Tnt',Store,filled_sbj,active,[node('EX',[node(Word,[])]),node('IP-INF-OB1',VL)|L],L) -->
  [w('EX',Word)],
  to_inf_layer(Store,there_sbj,active,VL,[]).
verb_complements_by_code(';~Tnt',Store,there_sbj,lgs_passive,[node('IP-INF-OB1',VL)|L],L) -->
  to_inf_layer(Store,there_sbj,active,VL,[]).
verb_complements_by_code(';~Tnt',Store,there_sbj,passive,[node('PP-LGS',[node('P-ROLE;_lgs_',[node(Word,[])]),NP]),node('IP-INF-OB1',VL)|L],L) -->
  [w('P-ROLE;_lgs_',Word)],
  noun_phrase('',non_privileged,[NP],[]),
  to_inf_layer(Store,there_sbj,active,VL,[]).

verb_complements_by_code(';~Tprt',Store,filled_sbj,active,L,L0) -->
  preposition_phrase('-DOB1',non_privileged,L,[node('IP-INF-OB1',VL)|L0]),
  to_inf_layer(Store,filled_sbj,active,VL,[]).

verb_complements_by_code(';~Tni',store([],[]),filled_sbj,active,L,L0) -->
  noun_phrase('-DOB1',non_privileged,L,[node('IP-INF-OB1',VL)|L0]),
  verb_phrase_layer(store([],[]),filled_sbj,infinitive,active,VL,[]).
verb_complements_by_code(';~Tni',store(Left,Right),filled_sbj,active,[node('NP-DOB1',[ICH]),node('IP-INF-OB1',VL)|L],L) -->
  {
    append(Left,Right,[np(ICH)])
  },
  verb_phrase_layer(store([],[]),filled_sbj,infinitive,active,VL,[]).

verb_complements_by_code(';~Tg',Store,filled_sbj,active,L,L0) -->
  ip_ppl_active('-OB1',Store,filled_sbj,ing_participle,L,L0).

verb_complements_by_code(';~Tng',store([],[]),filled_sbj,active,L,L0) -->
  noun_phrase('-DOB1',non_privileged,L,L1),
  ip_ppl_active('-OB1',store([],[]),filled_sbj,ing_participle,L1,L0).
verb_complements_by_code(';~Tng',store([np(ICH)],[]),filled_sbj,active,[node('NP-DOB1',[ICH])|L],L0) -->
  ip_ppl_active('-OB1',store([],[]),filled_sbj,ing_participle,L,L0).
verb_complements_by_code(';~Tng',store([],[]),filled_sbj,lgs_passive,L,L0) -->
  ip_ppl_active('-OB1',store([],[]),filled_sbj,ing_participle,L,L0).

verb_complements_by_code(';~Tsg',store([],[]),filled_sbj,active,[node('IP-PPL3-OB1',[node('NP-SBJ',NL)|VL])|L],L) -->
  noun_phrase_genm_layer(non_privileged,NL,[]),
  verb_phrase_layer(store([],[]),filled_sbj,ing_participle,active,VL,[]).
verb_complements_by_code(';~Tsg',store([],[]),filled_sbj,active,[node('IP-PPL3-OB1',[node('NP-SBJ',NL)|VL])|L],L) -->
  noun_phrase_top(non_privileged,NL,[]),
  verb_phrase_layer(store([],[]),filled_sbj,ing_participle,active,VL,[]).

% Ditransitive verb complements

verb_complements_by_code(';~Dn.n',store([],[]),filled_sbj,active,[node('NP-OB1',[node('PRO',[node('it',[])])])|L],L0) -->
  [w('PRO','it')],
  { ! },
  noun_phrase('-OB2',non_privileged,L,L0).
verb_complements_by_code(';~Dn.n',store([],[]),filled_sbj,active,L,L0) -->
  noun_phrase('-OB2',non_privileged,L,L1),
  noun_phrase('-OB1',non_privileged,L1,L0).
verb_complements_by_code(';~Dn.n',store(Left,Right),filled_sbj,active,[node('NP-OB1',[ICH])|L],L0) -->
  {
    append(Left,Right,[np(ICH)])
  },
  noun_phrase('-OB2',non_privileged,L,L0).
verb_complements_by_code(';~Dn.n',store([],[]),filled_sbj,lgs_passive,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,L0).
verb_complements_by_code(';~Dn.n',store(Left,Right),filled_sbj,lgs_passive,[node('NP-OB1',[ICH])|L],L) -->
  {
    append(Left,Right,[np(ICH)])
  },
  [].

verb_complements_by_code(';~Dn.f',Store,filled_sbj,active,L,L0) -->
  noun_phrase('-OB2',non_privileged,L,L1),
  cp_that('-OB1',_,Store,L1,L0).
verb_complements_by_code(';~Dn.f',Store,filled_sbj,lgs_passive,L,L0) -->
  cp_that('-OB1',_,Store,L,L0).

verb_complements_by_code(';~Dn.w',Store,filled_sbj,active,L,L0) -->
  noun_phrase('-OB2',non_privileged,L,L1),
  cp_embedded_que('-OB1',Store,L1,L0).
verb_complements_by_code(';~Dn.w',Store,filled_sbj,lgs_passive,L,L0) -->
  cp_embedded_que('-OB1',Store,L,L0).

verb_complements_by_code(';~Dn.r',store([],[]),filled_sbj,active,L,L0) -->
  noun_phrase('-OB2',non_privileged,L,L4),
  optional_punc_non_final(L4,L3),
  punc(left_quotation_mark,L3,L2),
  utterance('-OB1',L2,L1),
  punc(right_quotation_mark,L1,L0).
verb_complements_by_code(';~Dn.r',store([],[]),filled_sbj,lgs_passive,L,L0) -->
  optional_punc_non_final(L,L3),
  punc(left_quotation_mark,L3,L2),
  utterance('-OB1',L2,L1),
  punc(right_quotation_mark,L1,L0).

verb_complements_by_code(';~Dn.t',Store,filled_sbj,active,L,L0) -->
  noun_phrase('-OB2',non_privileged,L,[node('IP-INF-OB1',VL)|L0]),
  to_inf_layer(Store,filled_sbj,active,VL,[]).
verb_complements_by_code(';~Dn.t',Store,filled_sbj,lgs_passive,[node('IP-INF2-OB1',VL)|L],L) -->
  to_inf_layer(Store,filled_sbj,active,VL,[]).

verb_complements_by_code(';~Dn.*',store([],[]),filled_sbj,active,L,L0) -->
  noun_phrase('-OB2',non_privileged,L,L0).

verb_complements_by_code(';~Dn.pr',store([],[]),filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,L1),
  preposition_phrase('-OB2',non_privileged,L1,L0).
verb_complements_by_code(';~Dn.pr',store([],[]),filled_sbj,active,L,L0) -->
  preposition_phrase('-OB2',non_privileged,L,L1),
  noun_phrase('-OB1',non_privileged,L1,L0).
verb_complements_by_code(';~Dn.pr',store(Left,Right),filled_sbj,active,[node('NP-OB1',[ICH])|L],L0) -->
  {
    append(Left,Right,[np(ICH)])
  },
  preposition_phrase('-OB2',non_privileged,L,L0).
verb_complements_by_code(';~Dn.pr',store(Left,Right),filled_sbj,active,[node('PP-OB2',[ICH])|L],L0) -->
  {
    append(Left,Right,[pp(ICH)])
  },
  noun_phrase('-OB1',non_privileged,L,L0).
verb_complements_by_code(';~Dn.pr',store(Left,Right),filled_sbj,active,L,L0) -->
  {
    append(Left,Right,[np(ICH)])
  },
  noun_phrase('-OB1',non_privileged,L,[node('PP-OB2',[Role,node('NP',[ICH])])|L0]),
  role([Role],[]).
verb_complements_by_code(';~Dn.pr',store([],[]),filled_sbj,lgs_passive,L,L0) -->
  preposition_phrase('-OB2',non_privileged,L,L0).
verb_complements_by_code(';~Dn.pr',store(Left,Right),filled_sbj,lgs_passive,[node('PP-OB2',[ICH])|L],L) -->
  {
    append(Left,Right,[pp(ICH)])
  },
  [].
verb_complements_by_code(';~Dn.pr',store(Left,Right),filled_sbj,lgs_passive,[node('PP-OB2',[Role,node('NP',[ICH])])|L],L) -->
  {
    append(Left,Right,[np(ICH)])
  },
  role([Role],[]).

verb_complements_by_code(';~Dpr.f',Store,filled_sbj,active,L,L0) -->
  preposition_phrase('-OB2',non_privileged,L,L1),
  cp_that('-OB1',_,Store,L1,L0).

verb_complements_by_code(';~Dpr.r',store([],[]),filled_sbj,active,L,L0) -->
  preposition_phrase('-OB2',non_privileged,L,L4),
  optional_punc_non_final(L4,L3),
  punc(left_quotation_mark,L3,L2),
  utterance('-OB1',L2,L1),
  punc(right_quotation_mark,L1,L0).

% Complex-transitive verb complements

verb_complements_by_code(';~Cn.a',store([],[]),filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,L1),
  adjective_phrase('-PRD',store([],[]),non_privileged,L1,L0).
verb_complements_by_code(';~Cn.a',store([],[]),filled_sbj,active,L,L0) -->
  adjective_phrase('-PRD',store([],[]),non_privileged,L,L1),
  noun_phrase('-OB1',non_privileged,L1,L0).
verb_complements_by_code(';~Cn.a',store(Left,Right),filled_sbj,active,[node('NP-OB1',[ICH])|L],L0) -->
  {
    append(Left,Right,[np(ICH)])
  },
  adjective_phrase('-PRD',store([],[]),non_privileged,L,L0).
verb_complements_by_code(';~Cn.a',store([],[]),filled_sbj,active,[node('NP-OB1',[node('PRO;_provisional_',[node(Word,[])])])|L],L0) -->
  [w('PRO;_provisional_',Word)],
  adjective_phrase('-PRD',store([],[]),non_privileged,L,L1),
  notional_item3('-NOB1',L1,L0).
verb_complements_by_code(';~Cn.a',Store,filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,L2),
  adjective_phrase('-PRD',store([],[]),non_privileged,L2,[node('IP-INF-NOB1',VL)|L0]),
  to_inf_layer(Store,filled_sbj,passive,VL,[]).
verb_complements_by_code(';~Cn.a',store(Left,Right),filled_sbj,active,[node('NP-OB1',[ICH])|L],L0) -->
  {
    append(Left,Right,[np(ICH)])
  },
  adjective_phrase('-PRD',store([],[]),non_privileged,L,[node('IP-INF-NOB1',VL)|L0]),
  to_inf_layer(store([],[]),filled_sbj,passive,VL,[]).
verb_complements_by_code(';~Cn.a',store([],[]),filled_sbj,lgs_passive,L,L0) -->
  adjective_phrase('-PRD',store([],[]),non_privileged,L,L0).
verb_complements_by_code(';~Cn.a',store([],[]),filled_sbj,lgs_passive,L,L0) -->
  adjective_phrase('-PRD',store([],[]),non_privileged,L,[node('IP-INF-NOB1',VL)|L0]),
  to_inf_layer(store([],[]),filled_sbj,passive,VL,[]).

verb_complements_by_code(';~Cn.n',store([],[]),filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,L1),
  noun_phrase('-PRD',non_privileged,L1,L0).
verb_complements_by_code(';~Cn.n',store([],[]),filled_sbj,active,[node('NP-OB1',[node('PRO;_provisional_',[node(Word,[])])])|L],L0) -->
  [w('PRO;_provisional_',Word)],
  noun_phrase('-PRD',non_privileged,L,L1),
  notional_item3('-NOB1',L1,L0).
verb_complements_by_code(';~Cn.n',store(Left,Right),filled_sbj,active,[node('NP-OB1',[ICH])|L],L0) -->
  {
    append(Left,Right,[np(ICH)])
  },
  noun_phrase('-PRD',non_privileged,L,L0).
verb_complements_by_code(';~Cn.n',store([],[]),filled_sbj,lgs_passive,L,L0) -->
  noun_phrase('-PRD',non_privileged,L,L0).
verb_complements_by_code(';~Cn.n',store(Left,Right),filled_sbj,lgs_passive,[node('NP-PRD',[ICH])|L],L) -->
  {
    append(Left,Right,[np(ICH)])
  },
  [].

verb_complements_by_code(';~Cn.n/a',store([],[]),filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,[node('PP-PRD',[node('P-ROLE',[node('as',[])]),NP])|L0]),
  [w('P-ROLE','as')],
  noun_phrase('',non_privileged,[NP],[]).
verb_complements_by_code(';~Cn.n/a',store([],[]),filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,[node('PP-PRD',[node('P-ROLE',[node('as',[])]),ADJP])|L0]),
  [w('P-ROLE','as')],
  adjective_phrase('',store([],[]),non_privileged,[ADJP],[]).
verb_complements_by_code(';~Cn.n/a',store([],[]),filled_sbj,lgs_passive,[node('PP-PRD',[node('P-ROLE',[node('as',[])]),NP])|L],L) -->
  [w('P-ROLE','as')],
  noun_phrase('',non_privileged,[NP],[]).
verb_complements_by_code(';~Cn.n/a',store([],[]),filled_sbj,lgs_passive,[node('PP-PRD',[node('P-ROLE',[node('as',[])]),ADJP])|L],L) -->
  [w('P-ROLE','as')],
  adjective_phrase('',store([],[]),non_privileged,[ADJP],[]).
verb_complements_by_code(';~Cn.n/a',store([],[]),filled_sbj,passive,[node('PP-LGS',[node('P-ROLE;_lgs_',[node(Word,[])]),NP1]),node('PP-PRD',[node('P-ROLE',[node('as',[])]),NP2])|L],L) -->
  [w('P-ROLE;_lgs_',Word)],
  noun_phrase('',non_privileged,[NP1],[]),
  [w('P-ROLE','as')],
  noun_phrase('',non_privileged,[NP2],[]).
verb_complements_by_code(';~Cn.n/a',store([],[]),filled_sbj,passive,[node('PP-LGS',[node('P-ROLE;_lgs_',[node(Word,[])]),NP]),node('PP-PRD',[node('P-ROLE',[node('as',[])]),ADJP])|L],L) -->
  [w('P-ROLE;_lgs_',Word)],
  noun_phrase('',non_privileged,[NP],[]),
  [w('P-ROLE','as')],
  adjective_phrase('',store([],[]),non_privileged,[ADJP],[]).

verb_complements_by_code(';~Cn.pr',store([],[]),filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,L1),
  preposition_phrase('-PRD',non_privileged,L1,L0).
verb_complements_by_code(';~Cn.pr',store([],[]),filled_sbj,lgs_passive,L,L0) -->
  preposition_phrase('-PRD',non_privileged,L,L0).

verb_complements_by_code(';~Cn.t',Store,filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,[node('IP-INF-PRD',VL)|L0]),
  to_inf_layer(Store,filled_sbj,active,VL,[]).
verb_complements_by_code(';~Cn.t',store(Left,Right),filled_sbj,active,[node('NP-OB1',[ICH]),node('IP-INF-PRD',VL)|L],L) -->
  {
    append(Left,Right,[np(ICH)])
  },
  to_inf_layer(store([],[]),filled_sbj,active,VL,[]).
verb_complements_by_code(';~Cn.t',Store,filled_sbj,lgs_passive,[node('IP-INF-PRD',VL)|L],L) -->
  to_inf_layer(Store,filled_sbj,active,VL,[]).

verb_complements_by_code(';~Cn.i',Store,filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,[node('IP-INF-PRD',VL)|L0]),
  verb_phrase_layer(Store,filled_sbj,infinitive,active,VL,[]).
verb_complements_by_code(';~Cn.i',Store,filled_sbj,active,[node('EX',[node(Word,[])]),node('IP-INF-PRD',VL)|L],L) -->
  [w('EX',Word)],
  verb_phrase_layer(Store,there_sbj,infinitive,active,VL,[]).

verb_complements_by_code(';~Cn.g',Store,filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,L1),
  ip_ppl_active('-PRD',Store,filled_sbj,ing_participle,L1,L0).
verb_complements_by_code(';~Cn.g',Store,filled_sbj,lgs_passive,L,L0) -->
  ip_ppl_active('-PRD',Store,filled_sbj,ing_participle,L,L0).

% VP24A

verb_complements_by_code(';~VP24A',Store,filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,L1),
  ip_ppl_passive('IP-PPL-PRD',Store,filled_sbj,en_participle,L1,L0).
verb_complements_by_code(';~VP24A',Store,filled_sbj,lgs_passive,L,L0) -->
  ip_ppl_passive('IP-PPL-PRD',Store,filled_sbj,en_participle,L,L0).

% VP24B

verb_complements_by_code(';~VP24B',Store,filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,L1),
  ip_ppl_passive('IP-PPL-PRD',Store,filled_sbj,en_participle,L1,L0).

% VP24C

verb_complements_by_code(';~VP24C',Store,filled_sbj,active,L,L0) -->
  noun_phrase('-OB1',non_privileged,L,L1),
  ip_ppl_passive('IP-PPL-PRD',Store,filled_sbj,en_participle,L1,L0).

% Existential verb complements

verb_complements_by_code(';~ex_V',store([],[]),there_sbj,active,L,L0) -->
  noun_phrase('-ESBJ',established,L,L0).
verb_complements_by_code(';~ex_V',store(Left,Right),there_sbj,active,[node('NP-ESBJ',[ICH])|L],L) -->
  {
    append(Left,Right,[np(ICH)])
  },
  [].

verb_complements_by_code(';~ex_Vp',store([],[]),there_sbj,active,L,L0) -->
  noun_phrase('-ESBJ',established,L,L1),
  adverb_phrase('-CLR',particle,L1,L0).
verb_complements_by_code(';~ex_Vp',store(Left,Right),there_sbj,active,[node('NP-ESBJ',[ICH])|L],L0) -->
  {
    append(Left,Right,[np(ICH)])
  },
  adverb_phrase('-CLR',particle,L,L0).

verb_complements_by_code(';~ex_Vpr',store([],[]),there_sbj,active,L,L0) -->
  noun_phrase('-ESBJ',established,L,L1),
  preposition_phrase('-CLR',non_privileged,L1,L0).

verb_complements_by_code(';~ex_cat_Vt',Store,there_sbj,active,L,L0) -->
  noun_phrase('-ESBJ',established,L,[node('IP-INF-CAT',VL)|L0]),
  to(VL,VL1),
  verb_phrase_layer(Store,filled_sbj,infinitive,active,VL1,[]).
verb_complements_by_code(';~ex_cat_Vt',store([np(ICH)|Left],Right),there_sbj,active,[node('NP-ESBJ',[ICH]),node('IP-INF-CAT',VL)|L],L) -->
  to(VL,VL1),
  verb_phrase_layer(store(Left,Right),filled_sbj,infinitive,active,VL1,[]).

verb_complements_by_code(';~ex_cat_Vt_passive_',Store,there_sbj,active,L,L0) -->
  noun_phrase('-ESBJ',established,L,[node('IP-INF-CAT',VL)|L0]),
  {
    member(SbjType,[filled_sbj,unfilled_sbj])
  },
  to_inf_layer(Store,SbjType,passive,VL,[]).
verb_complements_by_code(';~ex_cat_Vt_passive_',store([np(ICH)|Left],Right),there_sbj,active,[node('NP-ESBJ',[ICH]),node('IP-INF-CAT',VL)|L],L) -->
  {
    member(SbjType,[filled_sbj,unfilled_sbj])
  },
  to_inf_layer(store(Left,Right),SbjType,passive,VL,[]).

verb_complements_by_code(';~ex_cat_Vg',Store,there_sbj,active,L,L0) -->
  noun_phrase('-ESBJ',established,L,L1),
  ip_ppl_active('-CAT',Store,filled_sbj,ing_participle,L1,L0).
verb_complements_by_code(';~ex_cat_Vg',store([np(ICH)|Left],Right),there_sbj,active,[node('NP-ESBJ',[ICH])|L],L0) -->
  ip_ppl_active('-CAT',store(Left,Right),filled_sbj,ing_participle,L,L0).

verb_complements_by_code(';~ex_cat_Ve_passive_',Store,there_sbj,active,L,L0) -->
  noun_phrase('-ESBJ',established,L,L1),
  ip_ppl_passive('IP-PPL-CAT',Store,filled_sbj,en_participle,L1,L0).
verb_complements_by_code(';~ex_cat_Ve_passive_',store([np(ICH)|Left],Right),there_sbj,active,[node('NP-ESBJ',[ICH])|L],L0) -->
  ip_ppl_passive('IP-PPL-CAT',store(Left,Right),filled_sbj,en_participle,L,L0).

% Equatives

verb_complements_by_code(';~equ_Vf',store([],[]),filled_sbj,active,L,L0) -->
  cp_that('-PRD',_,store([],[]),L,L0).

verb_complements_by_code(';~equ_Vw',store([],[]),filled_sbj,active,L,L0) -->
  cp_embedded_que('-PRD',store([],[]),L,L0).

verb_complements_by_code(';~equ_Vt',store([],[]),filled_sbj,active,L,L0) -->
  ip_to_inf('-PRD',store([],[]),L,L0).

verb_complements_by_code(';~equ_Vg',store([],[]),filled_sbj,active,[node('IP-PPL-PRD',IL)|L],L) -->
  ip_ppl_adverbial_layer(filled_sbj,ing_participle,IL,[]).
verb_complements_by_code(';~equ_Vg',store([],[]),filled_sbj,active,[node('IP-PPL3-PRD',IL)|L],L) -->
  ip_ppl_adverbial_layer(unfilled_sbj,ing_participle,IL,[]).

% Cleft

verb_complements_by_code(';~cleft_Vn',store([],[]),provisional_sbj,active,L,L0) -->
  noun_phrase('-FOC',non_privileged,L,[node('IP-CLF',IL)|L0]),
  relative_clause_finite_top_layer(IL,[]).
verb_complements_by_code(';~cleft_Vn',store(Left,Right),provisional_sbj,active,[node('NP-FOC',[ICH]),node('IP-CLF',IL)|L],L) -->
  {
    append(Left,Right,[np(ICH)])
  },
  relative_clause_finite_top_layer(IL,[]).

% V_as_though/as_if/like

verb_complements_by_code(';~V_as_though/as_if/like',Store,there_sbj,active,[node('PP-CLR',PL)|L],L) -->
  role(PL,[node('IP-ADV',IL)]),
  subject(there_sbj,IL,L1),
  clause_middle_layer(Store,there_sbj,L1,[]).
verb_complements_by_code(';~V_as_though/as_if/like',Store,filled_sbj,active,[node('PP-CLR',PL)|L],L) -->
  role(PL,[node('IP-ADV',IL)]),
  clause_top_layer(statement_order,Store,IL,[]).

% Catenative verb complements

verb_complements_by_code(';~cat_Vt',Store,SbjType,active,[node('IP-INF-CAT',VL)|L],L) -->
  {
    SbjType \== provisional_sbj
  },
  to(VL,VL1),
  verb_phrase_layer(Store,SbjType,infinitive,active,VL1,[]).

verb_complements_by_code(';~cat_Vt_passive_',Store,filled_sbj,active,[node('IP-INF-CAT',VL)|L],L) -->
  {
    member(SbjType,[filled_sbj,unfilled_sbj])
  },
  to_inf_layer(Store,SbjType,passive,VL,[]).

verb_complements_by_code(';~cat_Vi',Store,SbjType,active,[node('IP-INF-CAT',VL)|L],L) -->
  {
    SbjType \== provisional_sbj
  },
  verb_phrase_layer(Store,SbjType,infinitive,active,VL,[]).

verb_complements_by_code(';~cat_Vg',Store,SbjType,active,L,L0) -->
  {
    SbjType \== provisional_sbj
  },
  ip_ppl_active('-CAT',Store,SbjType,ing_participle,L,L0).

verb_complements_by_code(';~cat_Vg_passive_',Store,SbjType,active,L,L0) -->
  {
    SbjType \== provisional_sbj
  },
  ip_ppl_passive('IP-PPL-CAT',Store,SbjType,ing_participle,L,L0).

verb_complements_by_code(';~cat_Ve',Store,SbjType,active,L,L0) -->
  {
    SbjType \== provisional_sbj
  },
  ip_ppl_active('-CAT',Store,SbjType,en_participle,L,L0).

verb_complements_by_code(';~cat_Ve_passive_',Store,SbjType,active,L,L0) -->
  {
    SbjType \== provisional_sbj
  },
  ip_ppl_passive('IP-PPL-CAT',Store,SbjType,en_participle,L,L0).

% Clause top layer

clause_top_layer(statement_order,Store,L,L0) -->
  subject(SbjType,L,L1),
  clause_middle_layer(Store,SbjType,L1,L0).
clause_top_layer(imperative_clause,Store,L,L0) -->
  verb_phrase_layer(Store,filled_sbj,imperative,active,L,L0).
clause_top_layer(tag_question,store([],[]),L,L0) -->
  do_operator_layer(L,L1),
  subject(filled_sbj,L1,L0).
clause_top_layer(tag_question,store([],[]),L,L0) -->
  have_be_or_md_finite_layer(_,L,L1),
  subject(filled_sbj,L1,L0).
clause_top_layer(Type,Store,L,L0) -->
  {
    member(Type,[matrix_interrogative,matrix_constituent_interrogative,inversion_order])
  },
  do_operator_layer(L,L2),
  subject(SbjType,L2,L1),
  verb_phrase_layer(Store,SbjType,do_supported_infinitive,active,L1,L0).
clause_top_layer(Type,Store,L,L0) -->
  {
    member(Type,[matrix_interrogative,matrix_constituent_interrogative,inversion_order])
  },
  have_be_or_md_finite_layer(Code,L,L2),
  subject(SbjType,L2,L1),
  verb_complements_top_layer(Code,Store,SbjType,active,L1,L0).
clause_top_layer(matrix_constituent_interrogative,store([np(ICH)|Left],Right),[node('NP-SBJ',[ICH])|L],L0) -->
  {
    member(SbjType,[filled_sbj,derived_sbj])
  },
  clause_middle_layer(store(Left,Right),SbjType,L,L0).

clause_top_layer(Type,Store,L,L0) -->
  {
    ( Type==statement_order ->
      member(Type2,[statement_order,inversion_order])
    ;
      Type2=Type
    )
  },
  initial_adverbial(L,L2),
  optional_punc_non_final(L2,L1),
  clause_top_layer(Type2,Store,L1,L0).
clause_top_layer(Type,Store,L,L0) -->
  clause_top_layer(Type,Store,L,L2),
  optional_punc_non_final(L2,L1),
  adverbial(L1,L0).
clause_top_layer(matrix_interrogative,store(Left,Right),L,L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  interrogative_item(ICH,ICH_item,Index,L,L1),
  clause_top_layer(matrix_constituent_interrogative,store([ICH_item|Left],Right),L1,L0).
clause_top_layer(matrix_interrogative,Store,L,L0) -->
  adverb_phrase('-NIM',interrogative,L,L1),
  clause_top_layer(matrix_constituent_interrogative,Store,L1,L0).
clause_top_layer(statement_order,store(Left,Right),L,L0) -->
  {
    gensym('-',Index),
    atom_concat('-TPC',Index,Extra),
    atom_concat('*ICH*',Index,ICH)
  },
  displaced_item(ICH,ICH_item,Extra,L,L2),
  optional_punc_non_final(L2,L1),
  clause_top_layer(statement_order,store([ICH_item|Left],Right),L1,L0).
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

interrogative_item(ICH,np(node(ICH,[])),Ext,L,L0) -->
  noun_phrase(Ext,interrogative,L,L0).
interrogative_item(ICH,pp(node(ICH,[])),Ext,L,L0) -->
  preposition_phrase(Ext,interrogative,L,L0).
interrogative_item(ICH,pp_lgs(node(ICH,[])),Ext,[node(Label,[node('P-ROLE;_lgs_',[node(Word,[])]),NP])|L],L) -->
  {
    atom_concat('PP',Ext,Label)
  },
  [w('P-ROLE;_lgs_',Word)],
  noun_phrase('',interrogative,[NP],[]).
interrogative_item(ICH,adjp(node(ICH,[])),Ext,L,L0) -->
  adjective_phrase(Ext,store([],[]),interrogative,L,L0).
interrogative_item(ICH,advp(node(ICH,[])),Ext,L,L0) -->
  adverb_phrase(Ext,interrogative,L,L0).

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

ip_ppl_passive(Label,Store,SbjType,Infl,[node(Label,[node('NP-LGS',[node('*',[])])|VL])|L],L) -->
  verb_phrase_layer(Store,SbjType,Infl,lgs_passive,VL,[]).
ip_ppl_passive(Label,Store,SbjType,Infl,[node(Label,VL)|L],L) -->
  verb_phrase_layer(Store,SbjType,Infl,lgs_passive,VL,[node('PP-LGS',[node('P-ROLE;_lgs_',[node(Word,[])]),NP])]),
  [w('P-ROLE;_lgs_',Word)],
  noun_phrase('',non_privileged,[NP],[]).
ip_ppl_passive(Label,Store,SbjType,Infl,[node(Label,VL)|L],L) -->
  {
    (
      Store = store([pp_lgs(ICH)|Left],Right)
    ;
      Store = store(Left,[pp_lgs(ICH)|Right])
    )
  },
  verb_phrase_layer(store(Left,Right),SbjType,Infl,lgs_passive,VL,[node('PP-LGS',[ICH])]).
ip_ppl_passive(Label,Store,SbjType,Infl,[node(Label,VL)|L],L) -->
  verb_phrase_layer(Store,SbjType,Infl,passive,VL,[]).
ip_ppl_passive(Label,Store,SbjType,Infl,[node(Label,[node('ILYR',AL)])|L],L) -->
  ip_ppl_passive('ILYR',Store,SbjType,Infl,IL,[]),
  ip_ppl_passive_tail(Store,SbjType,Infl,CL,[]),
  { append(IL,CL,AL) }.

ip_ppl_passive_tail(Store,SbjType,Infl,[node('CONJP',AL)|L],L) -->
  conj(CONJ),
  ip_ppl_passive('ILYR',Store,SbjType,Infl,IL,[]),
  { append([CONJ],IL,AL) }.
ip_ppl_passive_tail(Store,SbjType,Infl,[PU,node('CONJP',IL)|L],L0) -->
  punc(non_final,[PU],[]),
  ip_ppl_passive('ILYR',Store,SbjType,Infl,IL,[]),
  ip_ppl_passive_tail(Store,SbjType,Infl,L,L0).

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
  adverb_phrase('-NIM',established,L,L1),
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

clause_that_layer(with_c,Store,L,L0) -->
  comp(L,L1),
  clause_top_layer(statement_order,Store,L1,L0).
clause_that_layer(without_c,Store,L,L0) -->
  clause_top_layer(statement_order,Store,L,L0).
clause_that_layer(without_c,store([np(ICH)|Left],Right),[node('NP-SBJ',[ICH])|L],L0) -->
  {
    member(SbjType,[filled_sbj,derived_sbj])
  },
  clause_middle_layer(store(Left,Right),SbjType,L,L0).

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
  verb_phrase_layer(Store,SbjType,infinitive,lgs_passive,L1,L0).
to_inf_layer(Store,SbjType,Voice,L,L0) -->
  {
    Voice == lgs_passive
  },
  to(L,L1),
  verb_phrase_layer(Store,SbjType,infinitive,Voice,L1,L0).
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

cp_embedded_que3(Ext,Store,[node(Label,[node('IP-SUB',IL)])|L],L) -->
  {
    atom_concat('CP-QUE',Ext,Label)
  },
  clause_embedded_que_finite_top_layer(Store,IL,[]).
cp_embedded_que3(Ext,Store,[node(Label,[node('IP-SUB',[node('ILYR',[node('ILYR',IL)|CL])])])|L],L) -->
  {
    atom_concat('CP-QUE',Ext,Label)
  },
  clause_embedded_que_finite_top_layer(Store,IL,[]),
  cp_embedded_que_finite_tail(Store,CL,[]).
cp_embedded_que3(Ext,Store,[node(Label,[node('IP-INF3',IL)])|L],L) -->
  {
    atom_concat('CP-QUE',Ext,Label)
  },
  clause_embedded_que_to_inf_top_layer(Store,IL,[]).
cp_embedded_que3(Ext,Store,[node(Label,[node('IP-INF3',[node('ILYR',[node('ILYR',IL)|CL])])])|L],L) -->
  {
    atom_concat('CP-QUE',Ext,Label)
  },
  clause_embedded_que_to_inf_top_layer(Store,IL,[]),
  cp_embedded_que_to_inf_tail(Store,CL,[]).

clause_embedded_que_finite_top_layer(Store,L,L0) -->
  comp_wq(L,L1),
  clause_top_layer(statement_order,Store,L1,L0).
clause_embedded_que_finite_top_layer(store(Left,Right),L,L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  noun_phrase(Index,interrogative,L,L1),
  clause_embedded_que_finite_lower_layer(store([np(node(ICH,[]))|Left],Right),L1,L0).
clause_embedded_que_finite_top_layer(store(Left,Right),L,L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  adverb_phrase(Index,interrogative,L,L1),
  clause_top_layer(statement_order,store([advp(node(ICH,[]))|Left],Right),L1,L0).
clause_embedded_que_finite_top_layer(Store,L,L0) -->
  adverb_phrase('-NIM',interrogative,L,L1),
  clause_top_layer(statement_order,Store,L1,L0).

clause_embedded_que_finite_lower_layer(store([np(ICH)|Left],Right),[node('NP-SBJ',[ICH])|L],L0) -->
  {
    member(SbjType,[filled_sbj,derived_sbj])
  },
  clause_middle_layer(store(Left,Right),SbjType,L,L0).
clause_embedded_que_finite_lower_layer(Store,L,L0) -->
  clause_top_layer(statement_order,Store,L,L0).

clause_embedded_que_to_inf_top_layer(Store,L,L0) -->
  comp_wq(L,L1),
  to_inf_layer(Store,filled_sbj,active,L1,L0).
clause_embedded_que_to_inf_top_layer(store(Left,Right),L,L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  noun_phrase(Index,interrogative,L,L1),
  to_inf_layer(store([np(node(ICH,[]))|Left],Right),filled_sbj,active,L1,L0).
clause_embedded_que_to_inf_top_layer(store(Left,Right),L,L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  adverb_phrase(Index,interrogative,L,L1),
  to_inf_layer(store([advp(node(ICH,[]))|Left],Right),filled_sbj,active,L1,L0).
clause_embedded_que_to_inf_top_layer(Store,L,L0) -->
  adverb_phrase('-NIM',interrogative,L,L1),
  to_inf_layer(Store,filled_sbj,active,L1,L0).

% notional item

notional_item(Ext,Store,L,L0) -->
  cp_that(Ext,_,Store,L,L0).
notional_item(Ext,Store,L,L0) -->
  cp_embedded_que(Ext,Store,L,L0).
notional_item(Ext,Store,L,L0) -->
  ip_to_inf(Ext,Store,L,L0).
notional_item(Ext,Store,L,L0) -->
  ip_ppl_active(Ext,Store,filled_sbj,ing_participle,L,L0).

notional_item3(Ext,L,L0) -->
  cp_that(Ext,_,store([],[]),L,L0).
notional_item3(Ext,L,L0) -->
  cp_embedded_que3(Ext,store([],[]),L,L0).
notional_item3(Ext0,L,L0) -->
  {
    atom_concat('3',Ext0,Ext)
  },
  ip_to_inf(Ext,store([],[]),L,L0).
notional_item3(Ext0,L,L0) -->
  {
    atom_concat('3',Ext0,Ext)
  },
  ip_ppl_active(Ext,store([],[]),filled_sbj,ing_participle,L,L0).

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
  conn(PL,[node('IP-INF2',VL)]),
  to_inf_layer(store([],[]),filled_sbj,active,VL,[]).
scon_clause([node('PP-SCON',PL)|L],L) -->
  conn(PL,[node('IP-PPL2',IL)]),
  {
    member(Infl,[hag_participle,en_participle,ing_participle])
  },
  ip_ppl_adverbial_layer(filled_sbj,Infl,IL,[]).
scon_clause([node('PP-SCON',PL)|L],L) -->
  conn(PL,[node('IP-ADV',IL)]),
  clause_top_layer(statement_order,store([],[]),IL,[]).
scon_clause([node('PP-SCON',PL)|L],L) -->
  conn(PL,[node('IP-PPL3',IL)]),
  {
    member(Infl,[hag_participle,en_participle,ing_participle])
  },
  ip_ppl_adverbial_layer(unfilled_sbj,Infl,IL,[]).

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
  ip_ppl_active('-CAT',store([],[]),SbjType,en_participle,L,L0).
ip_ppl_adverbial_layer(SbjType,Infl,L,L0) -->
  {
    Infl == ing_participle
  },
  verb_phrase_layer(store([],[]),SbjType,Infl,active,L,L0).
ip_ppl_adverbial_layer(SbjType,Infl,L,L0) -->
  {
    Infl == en_participle
  },
  verb_phrase_layer(store([],[]),SbjType,Infl,active,L,L0).
ip_ppl_adverbial_layer(SbjType,Infl,L,L0) -->
  {
    Infl == en_participle
  },
  verb_phrase_layer(store([],[]),SbjType,Infl,lgs_passive,L,[node('PP-LGS',[node('P-ROLE;_lgs_',[node(Word,[])]),NP])|L0]),
  [w('P-ROLE;_lgs_',Word)],
  noun_phrase('',non_privileged,[NP],[]).
ip_ppl_adverbial_layer(SbjType,Infl,[node('NP-LGS',[node('*',[])])|L],L0) -->
  {
    Infl == en_participle
  },
  verb_phrase_layer(store([],[]),SbjType,Infl,lgs_passive,L,L0).
ip_ppl_adverbial_layer(SbjType,Infl,L,L0) -->
  {
    Infl == en_participle
  },
  verb_phrase_layer(store([],[]),SbjType,Infl,passive,L,L0).
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
  ip_ppl_active('',store([],[]),filled_sbj,ing_participle,L,L0).
relative_clause(L,L0) -->
  ip_ppl_passive('IP-PPL',store([],[]),filled_sbj,en_participle,L,L0).

relative_clause_finite_top_layer(L,L0) -->
  comp(L,L1),
  relative_clause_finite_inside(store([np(node('*T*',[]))],[]),L1,L0).
relative_clause_finite_top_layer(L,L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  noun_phrase(Index,relative,L,L1),
  relative_clause_finite_inside(store([np(node(ICH,[]))],[]),L1,L0).
relative_clause_finite_top_layer(L,L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  preposition_phrase(Index,relative,L,L1),
  clause_top_layer(statement_order,store([pp(node(ICH,[]))],[]),L1,L0).
relative_clause_finite_top_layer([node(Label,[node('P-ROLE;_lgs_',[node(Word,[])]),NP])|L],L0) -->
  {
    gensym('-',Index),
    atom_concat('PP',Index,Label),
    atom_concat('*ICH*',Index,ICH)
  },
  [w('P-ROLE;_lgs_',Word)],
  noun_phrase('',relative,[NP],[]),
  clause_top_layer(statement_order,store([pp_lgs(node(ICH,[]))],[]),L,L0).
relative_clause_finite_top_layer(L,L0) -->
  preposition_phrase('-NIM',relative,L,L1),
  clause_top_layer(statement_order,store([],[]),L1,L0).
relative_clause_finite_top_layer(L,L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  adverb_phrase(Index,relative,L,L1),
  clause_top_layer(statement_order,store([advp(node(ICH,[]))],[]),L1,L0).
relative_clause_finite_top_layer(L,L0) -->
  adverb_phrase('-NIM',relative,L,L1),
  clause_top_layer(statement_order,store([],[]),L1,L0).
relative_clause_finite_top_layer(L,L0) -->
  clause_top_layer(statement_order,store([np(node('*T*',[]))],[]),L,L0).
relative_clause_finite_top_layer([node('NP-NIM',[node('*T*',[])])|L],L0) -->
  clause_top_layer(statement_order,store([],[]),L,L0).
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

relative_clause_finite_inside(store([np(ICH)|Left],Right),[node('NP-SBJ',[ICH])|L],L0) -->
  {
    member(SbjType,[filled_sbj,derived_sbj])
  },
  clause_middle_layer(store(Left,Right),SbjType,L,L0).
relative_clause_finite_inside(Store,L,L0) -->
  clause_top_layer(statement_order,Store,L,L0).

relative_clause_to_inf_top_layer(L,L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  preposition_phrase(Index,relative,L,L1),
  to_inf_top_layer(store([pp(node(ICH,[]))],[]),L1,L0).
relative_clause_to_inf_top_layer(L,L0) -->
  preposition_phrase('-NIM',relative,L,L1),
  to_inf_top_layer(store([],[]),L1,L0).
relative_clause_to_inf_top_layer(L,L0) -->
  to_inf_top_layer(store([np(node('*T*',[]))],[]),L,L0).
relative_clause_to_inf_top_layer([node('NP-SBJ',[node('*T*',[])])|L],L0) -->
  to_inf_layer(store([],[]),filled_sbj,active,L,L0).
relative_clause_to_inf_top_layer([node('NP-NIM',[node('*T*',[])])|L],L0) -->
  to_inf_top_layer(store([],[]),L,L0).
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

