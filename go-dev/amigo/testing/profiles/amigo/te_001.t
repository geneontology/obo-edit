{
    "id" : "te_001",
    "page" : "term_enrichment",
    "comment" : "Small TE set with IEAs.",
    "tests" : ["okay?", "code?", "links?"],
    "form" : "term_enrichment_form",
    "upload" : {
	"gp_file" : "/home/sjcarbon/local/src/svn/geneontology/go-dev/amigo/testing/data/sgd-small-10.txt",
	"bggp_file" : "/home/sjcarbon/local/src/svn/geneontology/go-dev/amigo/testing/data/sgd-small-40.txt"
    },
    "multi_select" : {"speciesdb" : ["SGD"]},
    "radio" : {"iea" : "yes"},
    "field" : {"cutoff" : "10"}
}
