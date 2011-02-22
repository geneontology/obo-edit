////////////
////
//// org.bbop.amigo.live_search
////
//// Purpose: Provide methods for accessing the results of the
////          AmiGO live search service.
////
//// DEPENDS: org.bbop.amigo
////
//////////


// A workspace object given a proper response from the server.
org.bbop.amigo.live_search = function(robj){

    var struct = robj;

    // Get the good bits.
    var total = struct.results.meta.total;
    var first = struct.results.meta.first;
    var last = struct.results.meta.last;
    var packet = struct.results.packet;
    var results = struct.results.hits;

    //
    this.total = function(){ return total; };
    this.first = function(){ return first; };
    this.packet = function(){ return packet; };
    this.last = function(){ return last; };
    this.results = function(){ return results; };
};
