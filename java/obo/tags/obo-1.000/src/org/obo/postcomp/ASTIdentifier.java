package org.obo.postcomp;

public class ASTIdentifier extends SimpleNode {
  protected String val;

  public ASTIdentifier(int id) {
    super(id);
  }

  public ASTIdentifier(OBOPostcomp p, int id) {
    super(p, id);
  }


  /** Accept the visitor. **/
  public Object jjtAccept(OBOPostcompVisitor visitor, Object data) {
    return visitor.visit(this, data);
  }
}
