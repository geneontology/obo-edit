{
    "id" : "te_003",
    "page" : "term_enrichment",
    "comment" : "Handling DOS line breaks.",
    "reference":"https://sourceforge.net/tracker/?func=detail&aid=2886837&group_id=36855&atid=908269",
    "tests" : ["okay?",
               "code?"],
    "form" : "term_enrichment_form",
    "upload" : {
	"gp_file" : "/home/sjcarbon/local/src/svn/geneontology/go-dev/amigo/testing/data/Willman_target.w32.smaller.txt",
	"bggp_file" : "/home/sjcarbon/local/src/svn/geneontology/go-dev/amigo/testing/data/Willman_background.w32.smaller.txt"
    },
    "multi_select" : {"speciesdb" : ["TAIR"]}
}
