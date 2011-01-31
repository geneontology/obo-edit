{
    "id" : "te_000",
    "page" : "term_enrichment",
    "comment" : "Image check through enrichment (check second file).",
    "tests" : ["okay?",
               "code?"],
    "form" : "term_enrichment_form",
    "upload" : {
	"gp_file" : "Willman_target.unix.1000.txt"
    },
    "multi_select" : {"speciesdb" : ["TAIR"]},
    "continue" : [
        {"id" : "te_000_sub_1",
         "comment" : "Image test on resultant page.",
         "tests" : ["okay?", "code?"],
         "form" : "visualize"}
    ]
}
