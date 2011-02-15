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
	"gp_file" : "sgd-small-10.txt",
	"bggp_file" : "sgd-small-40.txt"
    },
    "multi_select" : {"speciesdb" : ["SGD"]},
    "radio" : {"iea" : "no"},
    "field" : {"cutoff" : "10"}
}
