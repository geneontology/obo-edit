/* Generated By:JJTree: Do not edit this line. ASTFunctionDefinitions.java */

package org.bbop.expression.parser;

import org.apache.log4j.*;

public class ASTFunctionDefinitions extends SimpleNode {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ASTFunctionDefinitions.class);
  public ASTFunctionDefinitions(int id) {
    super(id);
  }

  public ASTFunctionDefinitions(Parser p, int id) {
    super(p, id);
  }


  /** Accept the visitor. **/
  @Override
public Object jjtAccept(ParserVisitor visitor, Object data) {
    return visitor.visit(this, data);
  }
}
