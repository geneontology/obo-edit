package org.oboedit.controller;

import java.lang.reflect.Method;

import org.bbop.expression.ExpressionException;
import org.bbop.expression.FunctionMappingImpl;
import org.bbop.expression.JexlContext;
import org.bbop.expression.context.HashMapContext;
import org.oboedit.script.ScriptUtil;
import org.oboedit.script.TermUtilScriptDelegate;

import org.apache.log4j.*;

public class ExpressionManager {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ExpressionManager.class);

	protected JexlContext context;

	protected static ExpressionManager manager;

	protected ExpressionManager() {
		context = new HashMapContext();
		initializeContext(context);
	}

	public JexlContext createSubContext(Object... params)
			throws ExpressionException {
		return createSubContext(context, true, true, params);
	}

	public static JexlContext createSubContext(JexlContext context,
			boolean global, boolean constants,
			Object... params) throws ExpressionException {
		if (params.length % 2 == 1)
			throw new IllegalArgumentException("There needs to be an even "
					+ "number of subcontext params");
		JexlContext out = context.createNewFrame();
		for (int i = 0; i < params.length; i += 2) {
			if (params[i] instanceof String) {
				if (global)
					out.setGlobalVariable((String) params[i], params[i + 1],
							constants);
				else
					out.setLocalVariable((String) params[i], params[i + 1],
							constants);
			} else
				throw new IllegalArgumentException(
						"createSubContext parameters should be "
								+ "an alternating list of strings and objects");
		}
		return out;
	}

	protected static void initializeContext(JexlContext context) {
		Class[] printArgs = { String.class };
		try {
			Method m = System.out.getClass().getMethod("print", printArgs);
			context.defineGlobalFunction("print", new FunctionMappingImpl(
					System.out, m));
			m = System.out.getClass().getMethod("println", printArgs);
			context.defineGlobalFunction("println", new FunctionMappingImpl(
					System.out, m));
			context.setGlobalVariable("TermUtil", new TermUtilScriptDelegate(),
					false);
			context.setGlobalVariable("Util", new ScriptUtil(), false);
		} catch (Exception ex) {
			// it'll always work, if it doesn't we're in a lot of trouble anyway
		}
	}

	public static ExpressionManager getManager() {
		if (manager == null)
			manager = new ExpressionManager();
		return manager;
	}

	public JexlContext getContext() {
		return context;
	}
}
