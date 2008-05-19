/*
 * Copyright 2002-2006 The Apache Software Foundation.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.bbop.expression.parser;

import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.math.BigInteger;

import org.bbop.expression.FunctionDef;
import org.bbop.expression.JexlContext;
import org.bbop.expression.util.Introspector;
import org.bbop.expression.util.introspection.Info;
import org.bbop.expression.util.introspection.VelMethod;

/**
 * Method execution.
 */
import org.apache.log4j.*;

public class ASTMethod extends SimpleNode {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ASTMethod.class);
	/** dummy velocity info. */
	private static final Info DUMMY = new Info("", 1, 1);

	/**
	 * Create the node given an id.
	 * 
	 * @param id
	 *            node id.
	 */
	public ASTMethod(int id) {
		super(id);
	}

	/**
	 * Create a node with the given parser and id.
	 * 
	 * @param p
	 *            a parser.
	 * @param id
	 *            node id.
	 */
	public ASTMethod(Parser p, int id) {
		super(p, id);
	}

	/** {@inheritDoc} */
	public Object jjtAccept(ParserVisitor visitor, Object data) {
		return visitor.visit(this, data);
	}

	public Object value(JexlContext jc) throws Exception {
		return execute(null, jc);
	}
	
	/**
	 * evaluate a method invocation upon a base object.
	 * 
	 * foo.bar(2)
	 * 
	 * @param jc
	 *            the {@link JexlContext} to evaluate against.
	 * @param obj
	 *            The object to have the method invoked.
	 * @return the value of the method invocation.
	 * @throws Exception
	 *             on any error
	 */
	public Object execute(Object obj, JexlContext jc) throws Exception {
		String methodName = ((ASTIdentifier) jjtGetChild(0)).val;

		int paramCount = jjtGetNumChildren() - 1;

		/*
		 * get our params
		 */

		Object[] params = new Object[paramCount];

		try {
			for (int i = 0; i < paramCount; i++) {
				params[i] = ((SimpleNode) jjtGetChild(i + 1)).value(jc);
			}

			FunctionDef def = null;
			if (obj == null)
				def = getEnclosingScript().getFunction(methodName, jc);
			if (def != null) {
		        try {
		        	return def.execute(jc, params, this);
		        } catch (ReturnValueException ex) {
		        	return ex.getValue();
		        }
			} else {
				VelMethod vm = Introspector.getUberspect().getMethod(obj,
						methodName, params, DUMMY);
				/*
				 * DG: If we can't find an exact match, narrow the parameters
				 * and try again!
				 */
				if (vm == null) {

					// replace all numbers with the smallest type that will fit
					for (int i = 0; i < params.length; i++) {
						Object param = params[i];
						if (param instanceof Number) {
							params[i] = narrow((Number) param);
						}
					}
					vm = Introspector.getUberspect().getMethod(obj, methodName,
							params, DUMMY);
					if (vm == null) {
						throw new BindException("Called unknown method "+methodName+" on object "+obj, getLineNum(), getCharNum());
					}
				}

				return vm.invoke(obj, params);
			}
		} catch (InvocationTargetException e) {
			Throwable t = e.getTargetException();

			if (t instanceof Exception) {
				throw (Exception) t;
			}

			throw e;
		}
	}

	/**
	 * Given a Number, return back the value using the smallest type the result
	 * will fit into. This works hand in hand with parameter 'widening' in java
	 * method calls, e.g. a call to substring(int,int) with an int and a long
	 * will fail, but a call to substring(int,int) with an int and a short will
	 * succeed.
	 * 
	 * @param original
	 *            the original number.
	 * @return a value of the smallest type the original number will fit into.
	 * @since 1.1
	 */
	private Number narrow(Number original) {
		if (original == null || original instanceof BigDecimal
				|| original instanceof BigInteger) {
			return original;
		}
		Number result = original;
		if (original instanceof Double || original instanceof Float) {
			double value = original.doubleValue();
			if (value <= Float.MAX_VALUE && value >= Float.MIN_VALUE) {
				result = new Float(result.floatValue());
			}
			// else it was already a double
		} else {
			long value = original.longValue();
			if (value <= Byte.MAX_VALUE && value >= Byte.MIN_VALUE) {
				// it will fit in a byte
				result = new Byte((byte) value);
			} else if (value <= Short.MAX_VALUE && value >= Short.MIN_VALUE) {
				result = new Short((short) value);
			} else if (value <= Integer.MAX_VALUE && value >= Integer.MIN_VALUE) {
				result = new Integer((int) value);
			}
			// else it was already a long
		}
		return result;
	}

}
