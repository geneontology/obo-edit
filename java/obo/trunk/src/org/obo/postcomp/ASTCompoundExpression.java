/* Generated By:JJTree: Do not edit this line. ASTCompoundExpression.java */

package org.obo.postcomp;

import org.apache.log4j.*;

public class ASTCompoundExpression extends SimpleNode {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ASTCompoundExpression.class);
  public ASTCompoundExpression(int id) {
    super(id);
  }

  public ASTCompoundExpression(OBOPostcomp p, int id) {
    super(p, id);
  }


  /** Accept the visitor. **/
  public Object jjtAccept(OBOPostcompVisitor visitor, Object data) {
    return visitor.visit(this, data);
  }
}
