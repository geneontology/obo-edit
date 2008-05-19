package org.obo.postcomp;

import org.apache.log4j.*;

public class ASTIdentifier extends SimpleNode {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ASTIdentifier.class);
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
