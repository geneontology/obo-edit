package org.bbop.expression.parser;

import java.util.LinkedList;
import java.util.List;

import org.bbop.expression.FunctionDef;
import org.bbop.expression.JexlContext;
import org.bbop.expression.JexlFunctionDefImpl;

import org.apache.log4j.*;

public class ASTFunctionDefinition extends SimpleNode {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ASTFunctionDefinition.class);

	public ASTFunctionDefinition(int i) {
		super(i);
	}

	public ASTFunctionDefinition(Parser p, int i) {
		super(p, i);
	}
	
    @Override
	public Object value(JexlContext jc) throws Exception {
        /* first child is the function name */
    	int nameIndex = 0;
    	boolean isGlobal = false;
    	if (jjtGetChild(0) instanceof ASTGlobalTag) {
    		nameIndex = 1;
    		isGlobal = true;
    	}
    	String methodName = ((ASTIdentifier) jjtGetChild(nameIndex)).val;
    	/* last child is actual method body
    	 */
    	ASTBlock body = (ASTBlock) jjtGetChild(jjtGetNumChildren() - 1);
 
    	List varNames = new LinkedList();
    	int paramCount = jjtGetNumChildren() - (2+nameIndex);
    	boolean isExtended = false;
        for (int i = 0; i < paramCount; i++) {
        	SimpleNode node = (SimpleNode) jjtGetChild(i+nameIndex+1);
        	if (node instanceof ASTIdentifier)
        		varNames.add(((ASTIdentifier) jjtGetChild(i+nameIndex+1)).val);
        	else if (node instanceof ASTExtendedParamIdentifier) {
        		varNames.add(((ASTExtendedParamIdentifier) jjtGetChild(i+nameIndex+1)).value(jc));
        		isExtended = true;
        	}
        }
        FunctionDef def = new JexlFunctionDefImpl(methodName, body, varNames, isExtended);
        if (isGlobal)
        	jc.defineGlobalFunction(methodName, def);
        else
        	getEnclosingScript().defineLocalFunction(jc, methodName, def);
//        jc.defineLocalFunction(methodName, new JexlFunctionDefImpl(methodName, body, varNames, isExtended));
 
    	return null;
    }    
}
