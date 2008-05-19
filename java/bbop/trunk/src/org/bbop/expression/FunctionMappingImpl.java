package org.bbop.expression;

import java.lang.reflect.Method;

import org.bbop.expression.parser.SimpleNode;

import org.apache.log4j.*;

public class FunctionMappingImpl implements FunctionMapping {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FunctionMappingImpl.class);

	protected Object callObject;
	protected Method method;
	
	public FunctionMappingImpl(Method method) {
		this(null, method);
	}
	
	public FunctionMappingImpl(Object callObject, Method method) {
		this.callObject = callObject;
		this.method = method;
	}
	
	public Object getCallObject() {
		return callObject;
	}

	public Method getMethod() {
		return method;
	}

	public Object execute(JexlContext jc, Object[] params, SimpleNode callNode) throws Exception {
		return method.invoke(getCallObject(), params);
	}

}
