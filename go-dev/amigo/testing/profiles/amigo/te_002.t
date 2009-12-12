{
    "id" : "te_002",
    "page" : "term_enrichment",
    "comment" : "Small TE set without IEAs.",
    "tests" : ["okay?",
               "code?"],
    "assertions" : [{"sub" : "NUMBER_OF_RESULTS",
                     "op" : ">=", "as" : "i",
                     "arg" : "300"}],
    "form" : "term_enrichment_form",
    "upload" : {
	"gp_file" : "/home/sjcarbon/local/src/svn/geneontology/go-dev/amigo/testing/data/sgd-small-10.txt",
	"bggp_file" : "/home/sjcarbon/local/src/svn/geneontology/go-dev/amigo/testing/data/sgd-small-40.txt"
    },
    "multi_select" : {"speciesdb" : ["SGD"]},
    "radio" : {"iea" : "no"},
    "field" : {"cutoff" : "10"}
}
