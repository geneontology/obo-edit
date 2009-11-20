/** Data model for storing and traversing Newick trees.
 */

/** NewickTree definition
 */

/** Newick tree constructor.
 *
 *  @param root - NewickNode instance to be the root of the tree
 */
function NewickTree(root) {
  this.root = root;
}

/** Get the root for the NewickTree object.
 *
 *  @return NewickNode for the root
 */
NewickTree.prototype.getRoot = function() {
  return this.root;
}

/** Set the root for the NewickTree object.
 *
 *  @param root - NewickNode to set as the root
 */
NewickTree.prototype.setRoot = function(root) {
  this.root = root;
}

/** NewickNode definition
 */

/** NewickNode constructor.
 *
 *  @param id - identifier for this NewickNode
 */
function NewickNode(id) {
  if (id) {
    this.id = id;
  }
  this.edges = [];
}

/** Get the identifier for the NewickNode object.
 *
 *  @return identifier for the node
 */
NewickNode.prototype.getId = function() {
  return this.id;
}

/** Set the identifier for the NewickNode object.
 *
 *  @param id - identifier for the node
 */
NewickNode.prototype.setId = function(id) {
  this.id = id;
}

/** Get the edges for this node.
 *
 *  @return array containing NewickEdge objects associated with the node
 */
NewickNode.prototype.getEdges = function() {
  return this.edges;
}

/** Get the children for this node.
 *
 *  @return array containing NewickNode objects for the children of the node.
 */
NewickNode.prototype.getChildren = function() {
  var children = [];
  for (var i = 0; i < this.getEdges().length; ++i) {
    var child = this.edges[i].getChild();
    children.push(child);
  }
  return children;
}

/** Add a child to the node.
 *
 *  @param node - node to add as a child.  If the child is a string,
 *                create an instance of a NewickNode object
 *  @param distance - distance to parent node
 *  @return NewickNode instance of the child
 */
NewickNode.prototype.addChild = function(node, distance) {
  var child = typeof node == "object" ? node : new NewickNode(node);
  this.edges.push(new NewickEdge(child, distance));
  return child;
}

/** NewickEdge definition
 */

/** NewickEdge constructor.
 *
 *  @param child - NewickNode instance for the child
 *  @param distance - Distance to parent
 */
function NewickEdge(child, distance) {
  this.child = child;
  this.distance = distance;
}

/** Get the child stored in the edge.
 *
 *  @return NewickNode instance for the child
 */
NewickEdge.prototype.getChild = function() {
  return this.child;
}

/** Get the distance from the child to the parent node.
 *
 *  @return distance from the child to the parent node
 */
NewickEdge.prototype.getDistance = function() {
  return this.distance;
}
