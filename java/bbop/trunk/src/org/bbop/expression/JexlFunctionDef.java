package org.bbop.expression;

import java.util.List;

import org.bbop.expression.parser.SimpleNode;

public interface JexlFunctionDef extends FunctionDef {
	
	public List getVarNames();
	public SimpleNode getNode();
	public boolean isExtendedParam();
}
