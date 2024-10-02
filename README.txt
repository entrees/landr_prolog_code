Code for the book "Exploring Layers and Ripples: Studying English with Computer Parsing" by Alastair Butler (Hirosaki University) available at https://ajb129.github.io/landr/

To run the program, you need a working XSB Tabling Prolog system (http://xsb.sourceforge.net/).

Example session
===============

$ xsb

| ?- [main].

| ?- tphrase_set_string([w('NS','boys')]), parse(noun).

| ?- tphrase_set_string([w('NS','boys')]), parse(noun_phrase('-SBJ',non_interrogative)).

| ?- tphrase_set_string([w('D','the'),w('N','ascent'),w('P-ROLE','of'),w('D','a'),w('ADJ','human'),w('N','figure'),w('P-ROLE','from'),w('D','the'),w('N','direction'),w('P-ROLE','of'),w('D','the'),w('ADJ','distant'),w('N','town')]), parse(noun_phrase('',non_privileged)).

| ?- tphrase_set_string([w('VB',';~I','look'),w('PUNC','!')]), parse(sentence).

| ?- tphrase_set_string([w('N','Cheese'),w('NS','pizzas'),w('VBD',Y,'smell'),w('PUNC','.')]), parse(sentence).

| ?- L = [w('ADJ',_),_,_,_], tphrase_set_string(L), tphrase(adjective_phrase('',non_privileged,R,[])), pp_ptree_list(R), fail.

| ?- L = [w(_,_,_),_,_,w('PUNC','?')], tphrase_set_string(L), tphrase(sentence(R,[])), pp_ptree_list(R), fail.

Licence
=======

All sources (c) 2024 Alastair Butler

This work is licensed under the Creative Commons Attribution
4.0 International License. To view a copy of this license, visit
http://creativecommons.org/licenses/by/4.0/ or send a letter to
Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

