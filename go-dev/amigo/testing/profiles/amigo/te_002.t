{
    "page" : "term_enrichment",
    "comment" : "testing for small set...no ieas",
    "tests" : ["okay?",
               "code?"],
    "assertions" : [{"sub" : "NUMBER_OF_RESULTS",
                     "op" : ">=", "as" : "i",
                     "arg" : "100"}],
    "form" : "term_enrichment_form",
    "upload" : {
	"gp_file" : "data/sgd-small-10.txt",
	"bggp_file" : "data/sgd-small-40.txt"
    },
    "multi_select" : {"speciesdb" : ["SGD"]},
    "radio" : {"iea" : "no"},
    "field" : {"cutoff" : "10"}
}
