////////////
////
//// org.bbop.amigo.ui.interactive
////
//// Purpose: Provide standard HTML UI production functions. Also adds
//// a few GUI elements that can be used as the app needs.
////          
//// Ajax widgets built on jQuery and automatically installed
//// into the document (hidden until used).
////
//// DEPENDS: org.bbop.amigo(.core)
//// DEPENDS: org.bbop.amigo.go_meta
//// DEPENDS: com.jquery (1.3.2)+?
////
//////////


// Module and namespace checking.
// TODO: Will we need a ui class up there?
if ( typeof org.bbop.amigo.ui == "undefined" ){
    org.bbop.amigo.ui = {};
}
if ( typeof org.bbop.amigo.ui.interactive == "undefined" ){
    org.bbop.amigo.ui.interactive = {};
}


//
org.bbop.amigo.ui.interactive.multi = function(in_id, in_name, in_size,
					       in_data, in_label){

    var anchor = this; // top-level this

    // Bring in utilities.
    var core = new org.bbop.amigo.core();
    var meta = new org.bbop.amigo.go_meta();

    // We'll be doing a lot of debugging.
    function ll(str){ core.kvetch(str); }
    ll("");

    // Copy in atom arguments.
    this.mid = in_id;
    this.mname = in_name;
    this.msize = in_size;
    this.mlabel = in_label;

    // Copy hash in safely.
    this.mdata = {};
    //this.mdata[core.util.randomness()] =
    this.mdata['_nil_'] =
	{
	    value: "",
	    label: "No Filter",
	    count: 0,
	    selected: true,
	    special: true
	};
    for( var ms = 0; ms < in_data.length; ms++ ){
	// this.mdata[core.util.randomness()] = // value should be unique
	this.mdata[in_data[ms][1]] =
	    {
		value: in_data[ms][1],
		label: in_data[ms][0],
		count: 0,
		selected: false,
		special: false
	    };
    }

    // Update the underlying data structure.
    // Can change count or selected.
    this.update_data = function(inkey, intype, inval){
	var rval = false;
	if( typeof anchor.mdata[inkey] != 'undefined' &&
	    typeof anchor.mdata[inkey][intype] != 'undefined' ){
		anchor.mdata[inkey][intype] = inval;
		rval = true;
	    }
	return rval;
    };

    function _render_label(){
	
	var buf = new Array();

	// Add the label.
	buf.push('<label for="');
	buf.push(anchor.mid);
	buf.push('" class="select">');
	buf.push(anchor.mlabel);
	buf.push('</label>');

	return buf.join('');	
    }
    this.render_label = _render_label;

    // Render our widget to a string.
    function _render_option(){

	var buf = new Array();

	// Sort the items in the mdata array.
	var mdata_keys = core.util.get_hash_keys(anchor.mdata);
	function _data_comp(a, b){

	    // Get the associated data.
	    var a_data = anchor.mdata[a];
	    var b_data = anchor.mdata[b];

	    //
	    var retval = 0;
	    if( a_data['special'] != b_data['special'] ){
		if( a_data['count'] == true ){	    
		    retval = 1;
		}else{
		    retval = -1;
		}
	    }else if( a_data['count'] != b_data['count'] ){
		if( a_data['count'] > b_data['count'] ){
		    retval = -1;
		}else{
		    retval = 1;
		}
	    }else if( a_data['label'] != b_data['label'] ){
		if( a_data['label'] > b_data['label'] ){
		    retval = 1;
		}else{
		    retval = -1;
		}
	    }

	    return retval;
	}
	mdata_keys.sort(_data_comp); // inplace sort

	//
	for( var mski = 0; mski < mdata_keys.length; mski++ ){
	    var write_data = anchor.mdata[mdata_keys[mski]];
	    buf.push('<option value="');
	    buf.push(write_data['value']);
	    if( write_data['selected'] ){
		buf.push('" selected="selected">');
	    }else{
		buf.push('">');		
	    }
	    buf.push(write_data['label']);
	    if( write_data['count'] && write_data['count'] > 0 ){
		buf.push(' (' + write_data['count'] + ')');
	    }
	    buf.push('</option>');
	}

	return buf.join('');
    }
    this.render_option = _render_option;


    // Render our widget to a string.
    function _render_select(){

	var buf = new Array();

	//
	buf.push('<select id="');
	buf.push(anchor.mid);
	buf.push('" name="');
	buf.push(anchor.mname);
	buf.push('" multiple size="');
	buf.push(anchor.msize);
	buf.push('">');
	
	buf.push(_render_option());

	//
	buf.push('</select>');

	return buf.join('');
    }
    this.render_select = _render_select;


    // Initial rendering of our widget to a string.
    this.render = function(){

	var buf = new Array();

	buf.push(_render_label());
	buf.push(_render_select());

	return buf.join('');
    };

    // ...
    this.update_gui = function(){
	jQuery('#' + anchor.mid).html(_render_option());
    };

    // // Create listener for actions on the form.
    // this.form.create_jquery_marshal = function(form_id, form_fields){

    // 	return function(event){

    // 	    // These will actually be variables used during function
    // 	    // generation in final form.
    // 	    //var form_id = '#app-form';
    // 	    //var form_fields = ['input', 'option:selected'];

    // 	    // Create jQuery selector string.
    // 	    var minibuf = new Array();
    // 	    for( var q = 0; q < form_fields.length ; q++ ){
    // 		minibuf.push( form_id + ' ' + form_fields[q] );
    // 	    }
    // 	    var selector_string = minibuf.join(', ');
    // 	    // ll('selector string: ' + selector_string);
	    
    // 	    //
    // 	    //var found = new Array();
    // 	    var found = {};
    // 	    var form_inputs = jQuery(selector_string);
    // 	    form_inputs.each(function(i, item) {
	    
    // 		var value = jQuery(item).val();
    // 		var name = null;
    // 		if( item.name ){
    // 		    name = item.name;
    // 		}else{
    // 		    name = jQuery(item).parent().attr('name');
    // 		}
    // 		// ll('_'+ i +':'+ name +':'+ value);

    // 		if( ! found[name] ){
    // 		    found[name] = [];
    // 		}
    // 		found[name].push(value);
    // 	    });
    // 	    return found;
    // 	};
    // };
};

    
