{
    "comment" : "testing for small set...no ieas",
    "assertions" : [["NUMBER_OR_RESULTS", "i_>=", "100"]],
    "form" : "term_enrichment_form",
    "upload" : {
	"gp_file" : "data/sgd-small-10.txt",
	"bggp_file" : "data/sgd-small-40.txt"
    },
    "multi_select" : {"speciesdb" : ["SGD"]},
    "radio" : {"iea" : "no"},
    "field" : {"cutoff" : "10"}
}
