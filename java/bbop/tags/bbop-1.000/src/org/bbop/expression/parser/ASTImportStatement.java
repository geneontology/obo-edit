/* Generated By:JJTree: Do not edit this line. ASTImportStatement.java */

package org.bbop.expression.parser;

public class ASTImportStatement extends SimpleNode {
  public ASTImportStatement(int id) {
    super(id);
  }

  public ASTImportStatement(Parser p, int id) {
    super(p, id);
  }


  /** Accept the visitor. **/
  public Object jjtAccept(ParserVisitor visitor, Object data) {
    return visitor.visit(this, data);
  }
}
