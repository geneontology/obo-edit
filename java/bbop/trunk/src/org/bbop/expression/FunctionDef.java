package org.bbop.expression;

import org.bbop.expression.parser.SimpleNode;

public interface FunctionDef {
	public Object execute(JexlContext jc, Object[] params, SimpleNode callNode) throws Exception;
}
