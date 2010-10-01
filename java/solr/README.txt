####
#### For installation, see INSTALL.txt in this directory.
####
#### TODO/BUG: I can't get dismax to work as I want, or really at
#### all--still slumping along with hard-coded standard...

## Also see golr.el in this directory.

## To update (redo) index:
http://accordion.lbl.gov:8080/solr/dataimport?command=full-import

## To query over multiple fields use:
http://wiki.apache.org/solr/DisMaxQParserPlugin
http://wiki.apache.org/solr/SolrRelevancyFAQ

## For JSONP uses:
http://accordion.lbl.gov:8080/solr/select/?q=neurogenesis&version=2.2&start=0&rows=10&indent=on&wt=json&json.wrf=functionbtl

###
### Other search handlers.
###

It seems like it's best to do our own munging on the client side to
get term completion until a better component comes out...

## To get the spell index ready:
http://accordion.lbl.gov:8080/solr/spell?q=pigments&spellcheck=true&spellcheck.collate=true&spellcheck.build=true
## To use the spell index:
http://accordion.lbl.gov:8080/solr/spell?q=pigments&spellcheck=true&spellcheck.collate=true
### But spellcheck not really useful here for autocomplete...
## How about:
http://accordion.lbl.gov:8080/solr/terms?terms.fl=name&terms.prefix=neurog
## Nice, but not there for multiple word entries...

###
### BENCHMARKING
###

CGI (ab -n 1000 -c 100 "http://accordion.lbl.gov/cgi-bin/amigo/completion?format=amigo&type=term&ontology=&narrow=false&query=nega neurogen")
   17471 14012
FCGI (ab -n 1000 -c 100 "http://accordion.lbl.gov/cgi-bin/amigo/completion?format=amigo&type=term&ontology=&narrow=false&query=nega neurogen")
   9327 5153 4137
Solr (ab -n 1000 -c 100 "http://accordion.lbl.gov:8080/solr/select?indent=on&version=2.2&q=nega*+neurogen*&fq=&start=0&rows=10&fl=*%2Cscore&qt=standard&wt=json&explainOther=&hl.fl=name")
   661 790 2641
