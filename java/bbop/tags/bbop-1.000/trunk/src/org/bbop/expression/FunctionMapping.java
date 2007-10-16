package org.bbop.expression;

import java.lang.reflect.Method;

public interface FunctionMapping extends FunctionDef {
	
	public Object getCallObject();
	public Method getMethod();

}
