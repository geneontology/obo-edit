////
//// ...
////


// Bring in the AmiGO core and keep a coder handy.
// TODO/BUG: switch DEBUG to false for release.
org.bbop.amigo.DEBUG = true;
var core = new org.bbop.amigo.core();
var gm = new org.bbop.amigo.go_meta();


var Manager = null;

//
function LiveSearchASInit(){

    core.kvetch('');
    core.kvetch('LiveSearchASInit start.');

    var surl = 'http://accordion.lbl.gov:8080/solr/';
    Manager = new AjaxSolr.Manager({solrUrl: surl}); 
    Manager.init();

    Manager.store.addByValue('q', '*:*');


    AjaxSolr.ResultWidget = AjaxSolr.AbstractWidget.extend({});
    Manager.addWidget(new AjaxSolr.ResultWidget({
						    id: 'results',
						    target: '#results_div'
						}));
    
    core.kvetch('LiveSearchASInit end.');
}
