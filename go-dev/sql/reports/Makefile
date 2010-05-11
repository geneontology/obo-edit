DB = go_latest_lite
DBHOST = spitz.lbl.gov
#DBHOST = 127.0.0.1
DBARGS = -d mysql:$(DB)@$(DBHOST)

REPORTS = sequence-reports reference-genome-reports

all: $(patsubst %,%.html,$(REPORTS))

%-$(DB).html: %.in.html
	go-process-report-template.pl $(DBARGS) $< > $@.tmp && mv $@.tmp $@

%.html: %.in.html
	go-process-report-template.pl $(DBARGS) $< > $@.tmp && mv $@.tmp $@

barplot.png:
	R --no-save x < go-barchart-basic.R && cp tmp.png $@

publish:
	cd .. && rsync -avz reports sos.lbl.gov:public_html/
