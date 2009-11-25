{
    "id" : "te_001",
    "page" : "term_enrichment",
    "comment" : "testing for small set with ieas",
    "tests" : ["okay?", "code?", "links?"],
    "form" : "term_enrichment_form",
    "upload" : {
	"gp_file" : "data/sgd-small-10.txt",
	"bggp_file" : "data/sgd-small-40.txt"
    },
    "multi_select" : {"speciesdb" : ["SGD"]},
    "radio" : {"iea" : "yes"},
    "field" : {"cutoff" : "10"}
}
