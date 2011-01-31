MAPS = mappings-GeneProduct-d2rq.n3 mappings-Term-d2rq.n3 
MAP-EXGO = mappings-ExGO-d2rq.n3

CONF-TEST = conf-test.n3
CONF-LIVE = conf-live.n3
CONF-EXGO = conf-exgo.n3

all: mappings-go-test.n3 mappings-go-live.n3 mappings-exgo.n3

mappings-go-test.n3: $(MAPS) $(CONF-TEST)
	cat  $(MAPS) $(CONF-TEST)  > $@

mappings-go-live.n3: $(MAPS) $(CONF-LIVE)
	cat  $(MAPS) $(CONF-LIVE) > $@

mappings-exgo.n3: $(MAPS) $(CONF-EXGO) $(MAP-EXGO)
	cat  $(MAPS) $(MAP-EXGO) $(CONF-EXGO) > $@
