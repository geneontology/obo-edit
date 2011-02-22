DB = go_latest_lite
#DBHOST = spitz.lbl.gov
DBHOST = 127.0.0.1
DBARGS = -d mysql:$(DB)@$(DBHOST)

REPORTS = sequence-reports reference-genome-reports

all: $(patsubst %,%.html,$(REPORTS))

%-$(DB).html: %.in.html
	go-process-report-template.pl $(DBARGS) $< > $@.tmp && mv $@.tmp $@

%.html: %.in.html
	go-process-report-template.pl $(DBARGS) $< > $@.tmp && mv $@.tmp $@

barplot.png:
	R --no-save x < go-barchart-basic.R && cp tmp.png $@

#publish:
#	cd .. && rsync -avz reports sos.lbl.gov:public_html/

queries: q-bg_proportion_of_genes_in_homolset_annotated_to

q-%.txt:
	mysql -h $(DBHOST) $(DB) -e "SELECT * FROM $*"  > $@
.PRECIOUS: q-%.txt

%.pro: q-%.txt
	tbl2p -ignore_hdr -p $* $< > $@

stats_proportion_of_%_in_homolset_annotated_to.txt: bg_proportion_of_%_in_homolset_annotated_to.pro
	blip -i $< -u stats_distributions findall -select "foo(H,T,GH,GHA,Prop,GB,GBA,P)" "(bg_proportion_of_$*_in_homolset_annotated_to(H,T,GH,GHA,Prop,GB,GBA),GH1 is GH-1,GHA1 is GHA-1,p_value_by_hypergeometric(GHA1,GH1,GBA,GB,P))" > $@.tmp && cut -f2-10 $@.tmp > $@
.PRECIOUS: %.txt

load-all: load-stats_proportion_of_genes_in_homolset_annotated_to load-stats_proportion_of_species_in_homolset_annotated_to
load-%: %.txt
	mysql -h $(DBHOST) $(DB) -e "DELETE FROM $*" ;\
	mysqlimport -L -h $(DBHOST) $(DB) $<

publish:
	rsync -avz *html sos.lbl.gov:public_html/reports
