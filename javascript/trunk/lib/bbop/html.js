/* 
 * Package: html.js
 * 
 * Namespace: bbop.html
 * 
 * Right now contains bbop.html.tag, but all html producing functions
 * should go in here somewhere.
 * 
 * All bbop.html implement the interface:
 *  .to_string(): returns a string of you and below
 *  .add_to(): add things between the tags
 *  .empty(): empties all things between the tags
 *  .get_id(): return the id or null if not defined
 * These are enforced during the tests.
 * 
 * For functions that take attribute hashes, there is a special
 * attribute {'generate_id': true} that will generate a somewhat
 * random id if an incoming id was not already specified. This id can
 * be retrieved using get_id().
 * 
 * This package takes all of the bbop.html.* namespace.
 */

bbop.core.require('bbop', 'core');
//bbop.core.require('bbop', 'logger');
//bbop.core.require('bbop', 'amigo');
bbop.core.namespace('bbop', 'html');
bbop.core.namespace('bbop', 'html', 'tag');
bbop.core.namespace('bbop', 'html', 'accordion');
bbop.core.namespace('bbop', 'html', 'list');
bbop.core.namespace('bbop', 'html', 'input');

/*
 * Namespace: bbop.html.tag
 * 
 * Constructor: tag
 * 
 * Create the fundamental tag object to work with and extend.
 * 
 * Parameters:
 *  tag - the tag name to be created
 *  attrs - *[serially optional]* the typical attributes to add
 *  below - *[optional]* a list/array of other html objects that exists "between" the tags
 * 
 * Returns:
 *  bbop.html.tag object
 */
bbop.html.tag = function(tag, attrs, below){
    this._is_a = 'bbop.html.tag';

    // Arg check--attrs should be defined as something.
    if( ! attrs ){ attrs = {}; }

    // Generate (or not) id if it was requested.
    if( ! bbop.core.is_defined(attrs['id']) &&
	bbop.core.is_defined(attrs['generate_id']) &&
	bbop.core.is_defined(attrs['generate_id']) == true ){
	    // Add a real id.
	    attrs['id'] = 'gen_id-bbop-html-'+ bbop.core.randomness(20);
	    // Remove the 'generated_id' property.
	    delete attrs['generate_id'];
	}
    this._attrs = attrs;
    
    // Arg check--below should be some kind of an array.
    if( ! below ){
	below = [];
    }else if( bbop.core.is_array(below) ){
	// do nothing
    }else{
	// hopefully a bbop.html.tag then
	below = [below];
    }

    // Accumulate the incoming attributes if there are any.
    var additional_attrs = '';
    bbop.core.each(this._attrs, function(in_key, in_val){
		       additional_attrs = additional_attrs + ' ' +
			   in_key + '="' + in_val + '"';
		   });

    this._car = '<' + tag + additional_attrs + '>';
    this._cdr = '</' + tag + '>';
    this._contents = below;
    this._singleton = '<' + tag + additional_attrs + ' />';
};

/*
 * Function: to_string
 * 
 * Convert a tag object into a html-ized string.
 * 
 * Parameters: n/a
 * 
 * Returns:
 *  string
 */
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

/*
 * Function: add_to
 * 
 * Add content between the tags. Order of addition is order of output.
 * 
 * Parameters:
 *  bbop_html_tag_or_string - another tag object or a string (html or otherwise)
 * 
 * Returns: n/a
 */
bbop.html.tag.prototype.add_to = function(bbop_html_tag_or_string){
    this._contents.push(bbop_html_tag_or_string);
};

/*
 * Function: empty
 * 
 * Remove all content between the tags.
 * 
 * Parameters: n/a
 * 
 * Returns: n/a
 */
bbop.html.tag.prototype.empty = function(){
    this._contents = [];
};

/*
 * Function: get_id
 * 
 * Return the id if extant, null otherwise.
 * 
 * Parameters: n/a
 * 
 * Returns: string or null
 */
bbop.html.tag.prototype.get_id = function(){
    var retval = null;
    if( bbop.core.is_defined(this._attrs['id']) ){
	retval = this._attrs['id'];
    }
    return retval;
};

/*
 * Namespace: bbop.html.accordion
 * 
 * Constructor: accordion
 * 
 * Create the a frame for the functional part of a jQuery accordion
 * structure.
 * 
 * :Input:
 * : [[title, string/*.to_string()], ...]
 * :
 * :Output:
 * : <div id="accordion">
 * :  <h3><a href="#">Section 1</a></h3>
 * :  <div>
 * :   <p>
 * :    foo
 * :   </p>
 * :  </div>
 * :  ...
 * : </div>
 * 
 * Parameters:
 *  in_list - accordion frame headers: [[title, string/*.to_string()], ...]
 *  attrs - *[serially optional]* attributes to apply to the new top-level div
 *  add_id_p - *[optional]* true or false; add a random id to each section
 * 
 * Returns:
 *  bbop.html.accordion object
 * 
 * Also see: <tag>
 */
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

/*
 * Function: to_string
 * 
 * Convert the accordion object into a html-ized string.
 * 
 * Parameters: n/a
 * 
 * Returns:
 *  string
 */
bbop.html.accordion.prototype.to_string = function(){
    return this._div_stack.to_string();
};

/*
 * Function: add_to
 * 
 * Add a contect section to the accordion.
 * 
 * Parameters:
 *  section_info - a string or a hash with 'id', 'label', and 'description'
 *  content_blob - string or bbop.html object to put in a section
 *  add_id_p - *[optional]* true or false; add a random id to the section
 * 
 * Returns: n/a
 */
bbop.html.accordion.prototype.add_to =
    function(section_info, content_blob, add_id_p){

    // If section_info isn't an object, assume it is a string and use
    // it for everything.
    var section_id = null;
    var section_label = null;
    var section_desc = null;
    if(typeof section_info != 'object' ){
	section_id = section_info;
	section_label = section_info;
    }else{
	if( section_info['id'] ){ section_id = section_info['id']; }
	if( section_info['label'] ){ section_label = section_info['label']; }
	if( section_info['description'] ){
	    section_desc = section_info['description'];
	}
    }

    // Add header section.
    var h3 = new bbop.html.tag('h3');
    var anc = null;
    if( section_desc ){
	anc = new bbop.html.tag('a', {href: '#', title: section_desc},
				section_label);
    }else{
	anc = new bbop.html.tag('a', {href: '#'}, section_label);
    }
    h3.add_to(anc);
    this._div_stack.add_to(h3);

    var div = null;

    // Generate random id for the div.
    if( typeof(add_id_p) == 'undefined' ){ add_id_p = false; }
    if( add_id_p ){
	var rid = 'accordion-' + section_id + '-' + bbop.core.randomness(20);
	this._section_id_to_content_id[section_id] = rid;    
	div = new bbop.html.tag('div', {'id': rid});	
    }else{
	div = new bbop.html.tag('div');	
    }

    // Add add content stub to section.
   var p = new bbop.html.tag('p', {}, bbop.core.to_string(content_blob));
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

/*
 * Function: empty
 * 
 * Empty all sections from the accordion.
 * 
 * Parameters: n/a
 * 
 * Returns: n/a
 */
bbop.html.accordion.prototype.empty = function(){
    this._div_stack = new bbop.html.tag('div', this._attrs);
    this._section_id_to_content_id = {};
};

/*
 * Function: get_id
 * 
 * Return the id if extant, null otherwise.
 * 
 * Parameters: n/a
 * 
 * Returns: string or null
 */
bbop.html.accordion.prototype.get_id = function(){
    return this._div_stack.get_id();
};

/*
 * Function: get_section_id
 * 
 * Get the "real" section id by way of the "convenience" section id?
 * 
 * Parameters:
 *  sect_id - TODO ???
 * 
 * Returns: TODO ???
 */
bbop.html.accordion.prototype.get_section_id = function(sect_id){
	return this._section_id_to_content_id[sect_id];    
};


// // TODO: just empty the contents from an ided section.
// bbop.html.accordion.prototype.empty_section = function(sect_id){
//     var div = this._section_id_to_content_div[sect_id];
//     div.empty();
// };

/*
 * Namespace: bbop.html.list
 * 
 * Constructor: list
 * 
 * Create the a frame for an unordered list object.
 * 
 * :Input:
 * : [string/*.to_string(), ...]
 * :
 * :Output:
 * : <ul id="list">
 * :  <li>foo</li>
 * :   ...
 * : </ul>
 * 
 * Parameters:
 *  in_list - list of strings/bbop.html objects to be li separated
 *  attrs - *[optional]* attributes to apply to the new top-level ul
 * 
 * Returns:
 *  bbop.html.list object
 * 
 * Also see: <tag>
 */
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

/*
 * Function: to_string
 * 
 * Convert a list object into a html-ized string.
 * 
 * Parameters: n/a
 * 
 * Returns:
 *  string
 */
bbop.html.list.prototype.to_string = function(){
    return this._ul_stack.to_string();
};

/*
 * Function: add_to
 * 
 * Add a new li section to a list.
 * 
 * Optionally, it can take multiple arguments and will add each of
 * them to the new li tag in turn.
 * 
 * Parameters:
 *  item1 - another tag object or a string (html or otherwise)
 *  item2 - *[optional]* ...on forever
 * 
 * Returns: n/a
 */
bbop.html.list.prototype.add_to = function(){

    // Convert anonymous arguments into an Array.
    var args = Array.prototype.slice.call(arguments); 

    // Cycle through and add them to the accumulator for the new li.
    var li_acc = [];
    bbop.core.each(args,
		   function(arg){
		       li_acc.push(bbop.core.to_string(arg));
		   });

    // Join them and add them to the stack of the encompassing ul.
    var li = new bbop.html.tag('li', {}, li_acc.join(" "));
    this._ul_stack.add_to(li);
};

/*
 * Function: empty
 * 
 * Remove all content (li's) from the list.
 * 
 * Parameters: n/a
 * 
 * Returns: n/a
 */
bbop.html.list.prototype.empty = function(){
    this._ul_stack = new bbop.html.tag('ul', this._attrs);
};

/*
 * Function: get_id
 * 
 * Return the id if extant, null otherwise.
 * 
 * Parameters: n/a
 * 
 * Returns: string or null
 */
bbop.html.list.prototype.get_id = function(){
    return this._ul_stack.get_id();
};

/*
 * Namespace: bbop.html.input
 * 
 * Constructor: input
 * 
 * Create a form input.
 * 
 * Parameters:
 *  attrs - *[optional]* the typical attributes to add
 * 
 * Returns:
 *  bbop.html.input object
 */
bbop.html.input = function(attrs){
    this._is_a = 'bbop.html.input';
    
    // Arg check--attrs should be defined as something.
    if( ! attrs ){ attrs = {}; }
    this._attrs = attrs;

    // Internal stack always starts with a ul.
    this._input_stack = new bbop.html.tag('input', this._attrs);
};

/*
 * Function: to_string
 * 
 * Convert an input into a html-ized string.
 * 
 * Parameters: n/a
 * 
 * Returns:
 *  string
 */
bbop.html.input.prototype.to_string = function(){
    return this._input_stack.to_string();
};

/*
 * Function: add_to
 * 
 * Add content between the input tags.
 * 
 * Parameters:
 *  item - another tag object or a string (html or otherwise)
 * 
 * Returns: n/a
 */
bbop.html.input.prototype.add_to = function(item){
    this._input_stack.add_to(bbop.core.to_string(item));
};

/*
 * Function: empty
 * 
 * Reset/remove all children.
 * 
 * Parameters: n/a
 * 
 * Returns: n/a
 */
bbop.html.input.prototype.empty = function(){
    this._input_stack = new bbop.html.tag('input', this._attrs);
};

/*
 * Function: get_id
 * 
 * Return the id if extant, null otherwise.
 * 
 * Parameters: n/a
 * 
 * Returns: string or null
 */
bbop.html.input.prototype.get_id = function(){
    return this._input_stack.get_id();
};

/*
 * Namespace: bbop.html.anchor
 * 
 * Constructor: anchor
 * 
 * Create an anchor object. Note: href, title, etc. go through
 * in_attrs.
 * 
 * Parameters:
 *  in_cont - the contents between the "a" tags
 *  in_attrs - *[optional]* the typical attributes to add
 * 
 * Returns:
 *  bbop.html.anchor object
 */
bbop.html.anchor = function(in_cont, in_attrs){
    this._is_a = 'bbop.html.anchor';
    
    // Arg check--attrs should be defined as something.
    this._attrs = in_attrs || {};

    // Internal stack always starts with a ul.
    this._anchor_stack = new bbop.html.tag('a', this._attrs, in_cont);
};

/*
 * Function: to_string
 * 
 * Convert an anchor object into a html-ized string.
 * 
 * Parameters: n/a
 * 
 * Returns:
 *  string
 */
bbop.html.anchor.prototype.to_string = function(){
    return this._anchor_stack.to_string();
};

/*
 * Function: add_to
 * 
 * Add content between the tags. Order of addition is order of output.
 * 
 * Parameters:
 *  item - another tag object or a string (html or otherwise)
 * 
 * Returns: n/a
 */
bbop.html.anchor.prototype.add_to = function(item){
    this._anchor_stack.add_to(item);
};

/*
 * Function: empty
 * 
 * Remove all content between the tags.
 * 
 * Parameters: n/a
 * 
 * Returns: n/a
 */
bbop.html.anchor.prototype.empty = function(){
    this._anchor_stack.empty();
};

/*
 * Function: get_id
 * 
 * Return the id if extant, null otherwise.
 * 
 * Parameters: n/a
 * 
 * Returns: string or null
 */
bbop.html.anchor.prototype.get_id = function(){
    return this._anchor_stack.get_id();
};

/*
 * Namespace: bbop.html.table
 * 
 * Constructor: table
 * 
 * Create a simple table structure.
 * in_headers is necessary, but can be empty.
 * in_entries is necessary, but can be empty.
 * 
 * Parameters:
 *  in_headers - ordered list of headers
 *  in_entries - lists of lists of entry items
 *  in_attrs - *[optional]* the typical attributes to add to the table
 * 
 * Returns:
 *  bbop.html.table object
 */
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

/*
 * Function: to_string
 * 
 * Convert a table object into a html-ized string.
 * 
 * Parameters: n/a
 * 
 * Returns:
 *  string
 */
bbop.html.table.prototype.to_string = function(){
    return this._table_stack.to_string();
};

/*
 * Function: add_to
 * 
 * Add data row. The entries argument is coerced into an array of tds.
 * 
 * Parameters:
 *  entries - lists of lists of entry items
 * 
 * Returns: n/a
 */
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

/*
 * Function: empty
 * 
 * Headers do not get wiped, just the data rows in the tbody.
 * 
 * Parameters: n/a
 * 
 * Returns: n/a
 */
bbop.html.table.prototype.empty = function(){
    this._count = 0;
    this._body_stack.empty();
};

/*
 * Function: get_id
 * 
 * Return the id if extant, null otherwise.
 * 
 * Parameters: n/a
 * 
 * Returns: string or null
 */
bbop.html.table.prototype.get_id = function(){
    return this._table_stack.get_id();
};

/*
 * Namespace: bbop.html.button
 * 
 * Constructor: button
 * 
 * Create a button object.
 * For after-the-fact decoration, take a look at:
 * <https://jquery-ui.googlecode.com/svn/tags/1.6rc5/tests/static/icons.html>
 * 
 * Parameters:
 *  in_label - label
 *  in_attrs - *[optional]* the typical attributes to add
 * 
 * Returns:
 *  bbop.html.button object
 */
bbop.html.button = function(in_label, in_attrs){
    this._is_a = 'bbop.html.button';
    
    // Arg check--attrs should be defined as something.
    this._attrs = in_attrs || {};

    // Internal stack is just the top-level button.
    this._button_stack = new bbop.html.tag('button', this._attrs, in_label);
};

/*
 * Function: to_string
 * 
 * Convert a button object into a html-ized string.
 * 
 * Parameters: n/a
 * 
 * Returns:
 *  string
 */
bbop.html.button.prototype.to_string = function(){
    return this._button_stack.to_string();
};

/*
 * Function: add_to
 * 
 * Add content between the tags. Order of addition is order of output.
 * Not really worth much as it just equates to changing the label.
 * 
 * Parameters:
 *  item - another tag object or a string (html or otherwise)
 * 
 * Returns: n/a
 */
bbop.html.button.prototype.add_to = function(item){
    this._button_stack.add_to(item);
};

/*
 * Function: empty
 * 
 * Remove all content between the tags. This equates to removing the
 * label.
 * 
 * Parameters: n/a
 * 
 * Returns: n/a
 */
bbop.html.button.prototype.empty = function(){
    this._button_stack.empty();
};

/*
 * Function: get_id
 * 
 * Return the id if extant, null otherwise.
 * 
 * Parameters: n/a
 * 
 * Returns: string or null
 */
bbop.html.button.prototype.get_id = function(){
    return this._button_stack.get_id();
};

/*
 * Namespace: bbop.html.span
 * 
 * Constructor: span
 * 
 * Create a span object.
 * Fun for calling live bits after the fact.
 * 
 * Parameters:
 *  in_label - label
 *  in_attrs - *[optional]* the typical attributes to add
 * 
 * Returns:
 *  bbop.html.span object
 */
bbop.html.span = function(in_label, in_attrs){
    this._is_a = 'bbop.html.span';
    
    // Arg check--attrs should be defined as something.
    this._attrs = in_attrs || {};

    // Internal stack is just the top-level span.
    this._span_stack = new bbop.html.tag('span', this._attrs, in_label);
};

/*
 * Function: to_string
 * 
 * Convert a span object into a html-ized string.
 * 
 * Parameters: n/a
 * 
 * Returns:
 *  string
 */
bbop.html.span.prototype.to_string = function(){
    return this._span_stack.to_string();
};

/*
 * Function: add_to
 * 
 * Add content between the tags. Order of addition is order of output.
 * Not really worth much as it just equates to changing the label.
 * 
 * Parameters:
 *  item - another tag object or a string (html or otherwise)
 * 
 * Returns: n/a
 */
bbop.html.span.prototype.add_to = function(item){
    this._span_stack.add_to(item);
};

/*
 * Function: empty
 * 
 * Remove all content between the tags. This equates to removing the
 * label.
 * 
 * Parameters: n/a
 * 
 * Returns: n/a
 */
bbop.html.span.prototype.empty = function(){
    this._span_stack.empty();
};

/*
 * Function: get_id
 * 
 * Return the id if extant, null otherwise.
 * 
 * Parameters: n/a
 * 
 * Returns: string or null
 */
bbop.html.span.prototype.get_id = function(){
    return this._span_stack.get_id();
};
