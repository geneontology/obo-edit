#!/usr/bin/python
# -*- coding: utf-8 -*-
####
#### WARNING: stopped partway--see tree2json.pl
####

from simpleparse.common import numbers
from simpleparse.parser import Parser

## EBNF declaration.
pthrjs_ebnf = r'''
file           := title_stanza, tree, gp_list 

title_stanza   := '[', 'title:', title_proper, ']', '\n'
title_proper   := [ a-zA-Z0-9\(\)\,\.\;\:\-\_\|\=]+

tree           := branch, crunch_box?, ';'

nhx_annotation := '[&&NHX:', *, ']'
nhx_tv_pair    := '[&&NHX:', *, ']'
nhx_tag        := '[&&NHX:', *, ']'
nhx_val        := '[&&NHX:', *, ']'
nhx_comment    := '[', ']'

gp_list        := gp_item*
gp_item        := gp_pipe_struct, ';', '\n'
gp_pipe_struct := symb_blob, '|', entrez_blob, '|', uniprot_blob
symb_blob      := simple_label, ':', simple_label
entrez_blob    := simple_label, '=', simple_label
uniprot_blob   := simple_label, '=', simple_label
simple_label   := [a-zA-Z0-9\-\_]+
'''
#newick_stanza  := string, '\n',
#simple_label   := [a-zA-Z0-9\-\_\=]+


##
if __name__ == '__main__':

    pthrjs_parser = Parser(pthrjs_ebnf, "file")

    mpath = '/home/sjcarbon/local/src/svn/geneontology/javascript/_data/'
    #for fn in ['trial1.txt']:
    for fn in ['PTHR10004.tree']:
        f = open(mpath + fn, 'r')
        f_text = f.read()
        print f_text

        print 'result:'
        print pthrjs_parser.parse(f_text)
