package org.bbop.expression;

import java.util.List;

import org.bbop.expression.parser.BindException;
import org.bbop.expression.parser.SimpleNode;

import org.apache.log4j.*;

public class JexlFunctionDefImpl implements JexlFunctionDef {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(JexlFunctionDefImpl.class);
	
	protected String name;
	protected List varNames;
	protected SimpleNode node;
	protected boolean isExtended;
	protected int lineNum;

	public JexlFunctionDefImpl(String name,
			SimpleNode node, List varNames, boolean isExtended) {
		this.name = name;
		this.node = node;
		this.varNames = varNames;
		this.isExtended = isExtended;
	}
	
	public SimpleNode getNode() {
		// TODO Auto-generated method stub
		return node;
	}

	public List getVarNames() {
		// TODO Auto-generated method stub
		return varNames;
	}
	
	public boolean isExtendedParam() {
		return isExtended;
	}

	public Object execute(JexlContext jc, Object[] params, SimpleNode callNode) throws Exception {
    	JexlContext newFrame = jc.createNewFrame();
    	
    	if (isExtended) {
    		try {
    			newFrame.setLocalVariable((String) getVarNames().get(0), params, true);
    		} catch (ExpressionException ex) {
    			ex.decorateException(node);
    		}
    	} else {
    		if (params.length > getVarNames().size())
    			throw new BindException("Too many arguments for function "+name, callNode.getLineNum(), callNode.getCharNum());
    		for(int i=0; i < getVarNames().size(); i++) {
    			newFrame.setLocalVariable((String) getVarNames().get(i), params[i], true);
    		}
    	}
    	return getNode().value(newFrame);
	}

}
