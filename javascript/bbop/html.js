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
};

//
bbop.html.tag.prototype.to_string = function(){
    var acc = '';
    bbop.core.each(this._contents,
		   function(item, i){
		       if( typeof(item) == 'string' ){
			   acc = acc + item;
		       }else{
			   acc = acc + item.to_string();
		       }
		   });
    return this._car + acc + this._cdr;
};

//
bbop.html.tag.prototype.add_child = function(bbop_html_tag_or_string){
    this._contents.push(bbop_html_tag_or_string);
};

//
bbop.html.tag.prototype.empty = function(){
    this._contents = [];
};


///
/// A jQuery accordion structure.
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
bbop.html.accordion = function(in_list, attrs){
    this._is_a = 'bbop.html.accordion';

    // Arg check--attrs should be defined as something.
    if( ! attrs ){ attrs = {}; }
    this._attrs = attrs;

    // Internal stack always starts with a div.
    this._div_stack = new bbop.html.tag('div', this._attrs);

    // Iterate over the incoming argument list.
    var accordion_this = this;
    bbop.core.each(in_list, function(item){
		       var title = item[0];
		       var content = item[1];
		       accordion_this.add_child(title, content);
		   });

};

//
bbop.html.accordion.prototype.to_string = function(){
    return this._div_stack.to_string();
};

//
bbop.html.accordion.prototype.add_child = function(title, content){

    // I keep forgetting this.
    if( ! title || ! content ){
	throw new Error('bbop.html.accordion.prototype.add_child 2 args');
    }
	
    // Add header section.
    var h3 = new bbop.html.tag('h3');
    var anc = new bbop.html.tag('a', {href: '#'}, title);
    h3.add_child(anc);
    this._div_stack.add_child(h3);
    
    // Add body section.
    var div = new bbop.html.tag('div');
    var p = new bbop.html.tag('p', {}, bbop.core.to_string(content));
    div.add_child(p);
    this._div_stack.add_child(div);
};

//
bbop.html.accordion.prototype.empty = function(){
    this._div_stack = new bbop.html.tag('div', this._attrs);
};

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
    bbop.core.each(in_list, function(item){ list_this.add_child(item); });
};

//
bbop.html.list.prototype.to_string = function(){
    return this._ul_stack.to_string();
};

// Add section.
bbop.html.list.prototype.add_child = function(item){
    var li = new bbop.html.tag('li', {}, bbop.core.to_string(item));
    this._ul_stack.add_child(li);
};

//
bbop.html.list.prototype.empty = function(){
    this._ul_stack = new bbop.html.tag('ul', this._attrs);
};
