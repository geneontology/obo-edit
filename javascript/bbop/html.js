////
//// Right now contains bbop.html.tag, but all html producing
//// functions should go in here somewhere.
////
//// All bbop.html implement the interface:
////   .output(): returns a string of you and below.
//// These are enforced during the tests.
////

bbop.core.require('bbop', 'core');
//bbop.core.require('bbop', 'logger');
//bbop.core.require('bbop', 'amigo');
bbop.core.namespace('bbop', 'html');
bbop.core.namespace('bbop', 'html', 'tag');
bbop.core.namespace('bbop', 'html', 'accordion');
bbop.core.namespace('bbop', 'html', 'list');
bbop.core.namespace('bbop', 'html', 'input');

///
/// bbop.html.tag--the fundamental unit that we'll work with.
///

//
bbop.html.tag = function(tag, attrs, below){
    this._is_a = 'bbop.html.tag';

    // Arg check--attrs should be defined as something.
    if( ! attrs ){ attrs = {}; }

    // Arg check--below should be some kind of an array.
    if( ! below ){
	below = [];
    }else if( bbop.core.is_array(below) ){
	// do nothing
    }else{
	// hopefully a bbop.html.tag
	below = [below];
    }

    // Accumulate the incoming attributes if there are any.
    var additional_attrs = '';
    bbop.core.each(attrs, function(in_key, in_val){
		       additional_attrs = additional_attrs + ' ' +
			   in_key + '="' + in_val + '"';
		   });

    this._car = '<' + tag + additional_attrs + '>';
    this._cdr = '</' + tag + '>';
    this._contents = below;
    this._singleton = '<' + tag + additional_attrs + ' />';
};

//
bbop.html.tag.prototype.to_string = function(){
    var acc = '';
    bbop.core.each(this._contents,
		   function(item, i){
		       // if( typeof(item) == 'string' ){
		       // 	   acc = acc + item;
		       // }else if( typeof(item['to_string']) == 'function' ){
		       // 	   acc = acc + item.to_string();
		       // }else{
		       // 	   throw new Error('No to_string for (' +
		       // 			   bbop.core.what_is(item) +
		       // 			   ') ' + item);
		       // }
		       acc = acc + bbop.core.to_string(item);
		   });
    
    // Special return case if there are no children (to prevent
    // weirdness for things like br and input).
    var output = this._singleton;
    if( acc != '' ){ output = this._car + acc + this._cdr; }

    return output;
};

//
bbop.html.tag.prototype.add_to = function(bbop_html_tag_or_string){
    this._contents.push(bbop_html_tag_or_string);
};

//
bbop.html.tag.prototype.empty = function(){
    this._contents = [];
};


///
/// The functional part of a jQuery accordion structure.
///

// [[title, string/*.to_string()], ...]
// <div id="accordion">
//   <h3><a href="#">Section 1</a></h3>
//   <div>
//     <p>
//       foo
//     </p>
//   </div>
//   ...
// </div>
bbop.html.accordion = function(in_list, attrs, add_id_p){
    this._is_a = 'bbop.html.accordion';

    //
    if( typeof(add_id_p) == 'undefined' ){ add_id_p = false; }

    // Arg check--attrs should be defined as something.
    this._attrs = attrs || {};

    // Internal stack always starts with a div.
    this._div_stack = new bbop.html.tag('div', this._attrs);

    this._section_id_to_content_id = {};

    // Iterate over the incoming argument list.
    var accordion_this = this;
    bbop.core.each(in_list, function(item){
		       var sect_title = item[0];
		       var content = item[1];
		       accordion_this.add_to(sect_title, content, add_id_p);
		   });
};

//
bbop.html.accordion.prototype.to_string = function(){
    return this._div_stack.to_string();
};

// Add a section to the accordion.
bbop.html.accordion.prototype.add_to = function(section_id, content, add_id_p){

    //
    if( typeof(add_id_p) == 'undefined' ){ add_id_p = false; }

    // Add header section.
    var h3 = new bbop.html.tag('h3');
    var anc = new bbop.html.tag('a', {href: '#'}, section_id);
    h3.add_to(anc);
    this._div_stack.add_to(h3);

    var div = null;

    // Generate random id for the div.
    if( add_id_p ){
	var rid = 'accordion-' + section_id + '-' + bbop.core.randomness(20);
	this._section_id_to_content_id[section_id] = rid;    
	div = new bbop.html.tag('div', {'id': rid});	
    }else{
	div = new bbop.html.tag('div');	
    }

    // Add add content stub to section.
   var p = new bbop.html.tag('p', {}, bbop.core.to_string(content));
    div.add_to(p);
    this._div_stack.add_to(div);
};

// // Add a section to the accordion.
// bbop.html.accordion.prototype.add_to_section = function(sect_id, content){
//     var cdiv = this._section_id_to_content_div[sect_id];
//     if( ! cdiv ){
// 	throw new Error('Cannot add to undefined section.');
//     }

// };

// Empty all sections from the accordion.
bbop.html.accordion.prototype.empty = function(){
    this._div_stack = new bbop.html.tag('div', this._attrs);
    this._section_id_to_content_id = {};
};

// 
bbop.html.accordion.prototype.get_section_id = function(sect_id){
	return this._section_id_to_content_id[sect_id];    
};


// // TODO: just empty the contents from an ided section.
// bbop.html.accordion.prototype.empty_section = function(sect_id){
//     var div = this._section_id_to_content_div[sect_id];
//     div.empty();
// };

///
/// An unordered list
///

// [string/*.to_string(), ...]
// <ul id="list">
//     <li>foo</li>
//   ...
// </ul>
bbop.html.list = function(in_list, attrs){
    this._is_a = 'bbop.html.list';
    
    // Arg check--attrs should be defined as something.
    if( ! attrs ){ attrs = {}; }
    this._attrs = attrs;

    // Internal stack always starts with a ul.
    this._ul_stack = new bbop.html.tag('ul', this._attrs);

    var list_this = this;
    bbop.core.each(in_list, function(item){ list_this.add_to(item); });
};

//
bbop.html.list.prototype.to_string = function(){
    return this._ul_stack.to_string();
};

// Add section.
bbop.html.list.prototype.add_to = function(item){
    var li = new bbop.html.tag('li', {}, bbop.core.to_string(item));
    this._ul_stack.add_to(li);
};

//
bbop.html.list.prototype.empty = function(){
    this._ul_stack = new bbop.html.tag('ul', this._attrs);
};

///
/// A form input.
///

//bbop.html.input = function(in_list, attrs){
bbop.html.input = function(attrs){
    this._is_a = 'bbop.html.input';
    
    // Arg check--attrs should be defined as something.
    if( ! attrs ){ attrs = {}; }
    this._attrs = attrs;

    // Internal stack always starts with a ul.
    this._input_stack = new bbop.html.tag('input', this._attrs);
};

//
bbop.html.input.prototype.to_string = function(){
    return this._input_stack.to_string();
};

// Add section.
bbop.html.input.prototype.add_to = function(item){
    this._input_stack.add_to(bbop.core.to_string(item));
};

// Reset/remove all children.
bbop.html.input.prototype.empty = function(){
    this._input_stack = new bbop.html.tag('input', this._attrs);
};

///
/// An anchor.
///

// href, title, etc. go through attrs
bbop.html.anchor = function(in_cont, in_attrs){
    this._is_a = 'bbop.html.anchor';
    
    // Arg check--attrs should be defined as something.
    this._attrs = in_attrs || {};

    // Internal stack always starts with a ul.
    this._anchor_stack = new bbop.html.tag('a', this._attrs, in_cont);
};

//
bbop.html.anchor.prototype.to_string = function(){
    return this._anchor_stack.to_string();
};

// Add section.
bbop.html.anchor.prototype.add_to = function(item){
    this._anchor_stack.add_to(item);
};

// ...
bbop.html.anchor.prototype.empty = function(){
    this._anchor_stack.empty();
};

///
/// A simple table structure.
///

// in_headers is necessary, but can be empty. As can in_entries.
bbop.html.table = function(in_headers, in_entries, in_attrs){
    this._is_a = 'bbop.html.table';
    
    // Arg check--attrs should be defined as something.
    var headers = in_headers || [];
    var entries = in_entries || [];
    var attrs = in_attrs || {};

    // Row class count.
    this._count = 0;

    // Internal stack always starts with a table.
    this._table_stack = new bbop.html.tag('table', this._attrs);

    // Only add headers if they exist.
    if( ! bbop.core.is_empty(headers) ){
	var head_row = new bbop.html.tag('tr');
	bbop.core.each(headers,
		       function(header){
			   var th = new bbop.html.tag('th');
			   th.add_to(header);
			   head_row.add_to(th);
		       });
	var head_stack = new bbop.html.tag('thead');
	head_stack.add_to(head_row);
	this._table_stack.add_to(head_stack);
    }

    // Add incoming rows to the body. Keep the body stack around for
    // bookkeeping.
    this._body_stack = new bbop.html.tag('tbody');
    this._table_stack.add_to(this._body_stack);

    var this_table = this;
    bbop.core.each(entries, function(item){ this_table.add_to(item); });
};

//
bbop.html.table.prototype.to_string = function(){
    return this._table_stack.to_string();
};

// Add data row. entries is coerced into an array of tds.
bbop.html.table.prototype.add_to = function(entries){
    
    //this._body_stack = new bbop.html.tag('tbody');

    // Get the class for the row.
    var row_class = 'odd_row';
    if( this._count % 2 == 0 ){ row_class = 'even_row'; }
    this._count = this._count + 1;

    var tr = new bbop.html.tag('tr', {'class': row_class});

    // Array or not, add everything as tds.
    if( ! bbop.core.is_array(entries) ){ entries = [entries]; }
    bbop.core.each(entries,
		   function(entry){
		       var td = new bbop.html.tag('td');
		       td.add_to(entry);
		       tr.add_to(td);
		   });
    this._body_stack.add_to(tr);
};

// Headers do not get wiped, just the data rows in the tbody.
bbop.html.table.prototype.empty = function(){
    this._count = 0;
    this._body_stack.empty();
};
