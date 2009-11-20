{
    "id" : "te_000",
    "page" : "term_enrichment",
    "comment" : "setting up for image test...",
    "tests" : ["okay?",
               "code?"],
    "form" : "term_enrichment_form",
    "upload" : {
	"gp_file" : "data/sgd-small-40.txt"
    },
    "field" : {
      "cutoff" : "10"
    },
    "multi_select" : {"speciesdb" : ["SGD"]},
    "continue" : [
        {"id" : "te_000 sub 1",
         "comment" : "image test on resultant page",
         "tests" : ["okay?", "code?"],
         "form" : "visualize"}
    ]
}
