{
    "id" : "te_000",
    "page" : "term_enrichment",
    "comment" : "Image check through enrichment (check second file).",
    "tests" : ["okay?",
               "code?"],
    "form" : "term_enrichment_form",
    "upload" : {
	"gp_file" : "/home/sjcarbon/local/src/svn/geneontology/go-dev/amigo/testing/data/Willman_target.unix.1000.txt"
    },
    "multi_select" : {"speciesdb" : ["TAIR"]},
    "continue" : [
        {"id" : "te_000 sub 1",
         "comment" : "Image test on resultant page.",
         "tests" : ["okay?", "code?"],
         "form" : "visualize"}
    ]
}
