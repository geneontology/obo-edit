/* Generated By:JJTree: Do not edit this line. ASTDifferentiaExpression.java */

package org.obo.postcomp;

public class ASTDifferentiaExpression extends SimpleNode {
  public ASTDifferentiaExpression(int id) {
    super(id);
  }

  public ASTDifferentiaExpression(OBOPostcomp p, int id) {
    super(p, id);
  }


  /** Accept the visitor. **/
  public Object jjtAccept(OBOPostcompVisitor visitor, Object data) {
    return visitor.visit(this, data);
  }
}
