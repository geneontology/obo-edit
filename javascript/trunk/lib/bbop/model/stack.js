/* 
 * Package: stack.js
 * 
 * Namespace: bbop.model.graph.stack
 * 
 * Purpose: An extension of <bbop.model.graph> to produce a stacked
 * layout (like the neighborhood view in miGO 1.8).
 * 
 * TODO: A work in progress...
 */

// Module and namespace checking.
bbop.core.require('bbop', 'core'); // not needed, but want the habit
bbop.core.require('bbop', 'model');
bbop.core.namespace('bbop', 'model', 'graph', 'stack');

/*
 * Namespace: bbop.model.graph.stack
 * 
 * Constructor: stack
 * 
 * Extension of <bbop.model.graph>
 * 
 * Arguments:
 *  n/a
 * 
 * Returns:
 *  this
 */
bbop.model.graph.stack = function(){
    bbop.model.graph.call(this);
    this._is_a = 'bbop.model.graph.stack';

    
};
bbop.model.graph.stack.prototype = new bbop.model.graph;