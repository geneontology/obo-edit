/** Provides different utilities for dealing with Newick trees.
 *  Requires NewickTree.js that defines the datamodel.
 *  Also requires json2.js (which already comes with Firefox).
 */

/** Namespace for the utility functions.
 */
function NewickTreeUtils() {
}

/** Convert a NewickTree object to its JSON representation.
 *
 *  @param tree - NewickTree object to convert
 *  @return string with JSON representation of the tree data
 */
NewickTreeUtils.toJSON = function(tree) {
  return JSON.stringify(tree.getRoot());
}

/** Parse a JSON string into a NewickTree object.
 *
 *  @param json - JSON data for the NewickTree
 *  @return NewickTree instance
 */
NewickTreeUtils.parseJSON = function(json) {
  var root = JSON.parse(json);
  NewickTreeUtils.addMethodsToNodeAndEdges(root);
  return new NewickTree(root);
}

/** Convert a NewickTree object to its Newick tree representation.
 *
 *  @param tree - NewickTree object to convert
 *  @return string with Newick representation of the tree data
 */
NewickTreeUtils.toNewick = function(tree) {
  return NewickTreeUtils.processNodeForNewick(tree.getRoot(), "", 0) + ";";
}

/** Parse a Newick string into a NewickTree object.
 *
 *  @param newick - Newick data for the NewickTree
 *  @return NewickTree instance
 */
NewickTreeUtils.parseNewick =  function(newick) {
  // Wrapper for the index while parsing the Newick string.
  // Need to wrap it in an object since JavaScript does not allow
  // passing primitive types by reference.
  function Index() {
    this.i = 0;
  }
  return new NewickTree(NewickTreeUtils.build(newick, 0, new Index())[0]);
}

/****** "Private" methods - shouldn't be called from outside script
 ******/

/** Parse a Newick string and create a NewickNode from the data.
 *  This function uses recursion to process descendants.
 */
NewickTreeUtils.build = function(s, from, i) {
  var node = new NewickNode();
  var id = "";
  var distance = 0;
  for (i.i = from; i.i < s.length; ++i.i) {
    var c = s.charAt(i.i);
    switch (c) {
      // new node
      case '(':
        do {
          var retVals = NewickTreeUtils.build(s, i.i + 1, i);
          node.addChild(retVals[0], retVals[1]);
        } while (s.charAt(i.i) == ','); // add siblings
        break;
      // end of current node, return it
      case ',':
      case ')':
        node.setId(id.toString());
        return [node, distance];
        break;
      // branch distance
      case ':':
          distance = NewickTreeUtils.getDistance(s, i.i + 1);
          i.i += distance.length;
          break;
      // done parsing Newick string
      case ';':
          break;
      // identifier, append to id buffer
      default:
        id += c;
        break;
    }
  }
  // return root node
  node.setId(id);
  return [node, distance];
}

/** Parse the branch distance from the Newick data.
 */
NewickTreeUtils.getDistance = function(s, start) {
  for (var i = start; i < s.length; ++i) {
    if (!isFinite(s.charAt(i)) && s.charAt(i) != '.') {
      break;
    }
  }
  return s.substring(start, i);
}

/** Add methods back to generic object.
 */
NewickTreeUtils.addMethods = function(obj, methods) {
  for (var method in methods) {
    obj[method] = methods[method];
  }
}

/** Add specific NewickNode and NewickEdge methods back to generic
 *  objects.
 */
NewickTreeUtils.addMethodsToNodeAndEdges = function(node) {
  NewickTreeUtils.addMethods(node, NewickNode.prototype);
  for (var i = 0; i < node.getEdges().length; ++i) {
    var edge = node.getEdges()[i];
    NewickTreeUtils.addMethods(edge, NewickEdge.prototype);
    NewickTreeUtils.addMethodsToNodeAndEdges(edge.getChild());
  }
}

/** Process each node and return Newick representation.
 */
NewickTreeUtils.processNodeForNewick = function(node, out, distance) {
  var edges = node.getEdges();
  if (edges.length > 0) {
    out += "(";
    for (var i = 0; i < edges.length; ++i) {
      var edge = edges[i];
      if (i > 0) {
        out += ",";
      }
      out = NewickTreeUtils.processNodeForNewick(edge.getChild(), out,
        edge.getDistance());
    }
    out += ")";
  }
  out += node.getId();
  if (distance > 0) {
    out += ":" + distance;
  }
  return out;
}
