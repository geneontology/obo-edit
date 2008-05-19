package org.bbop.expression;

import org.apache.log4j.*;

public class ExpressionUtil {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ExpressionUtil.class);
	private ExpressionUtil() {

	}

	public static String resolveBacktickExpression(String str,
			JexlContext context) throws ExpressionException {
		if (str == null)
			return null;
		if (context == null)
			return str;
		StringBuffer out = new StringBuffer();
		StringBuffer expression = new StringBuffer();
		StringBuffer writeToMe = out;
		boolean inBackticks = false;
		for (int i = 0; i < str.length(); i++) {
			char c = str.charAt(i);
			if (c == '\\') {
				if (i < str.length() - 1) {
					c = str.charAt(++i);
				}
				writeToMe.append(c);
			} else if (c == '`') {
				if (inBackticks) {
					String val = "`" + expression.toString() + "`";

					Object o = ExpressionUtil.exec(expression.toString(), context);
					val = o.toString();

					expression = new StringBuffer();
					writeToMe = out;
					writeToMe.append(val);
					inBackticks = false;
				} else {
					inBackticks = true;
					writeToMe = expression;
				}
			} else
				writeToMe.append(c);
		}
		return out.toString();
	}

	public static Object exec(String code, JexlContext context)
			throws ExpressionException {
		Script s = ScriptFactory.createScript(code);
		return s.execute(context);
	}

	public static int execInt(String code, JexlContext context)
			throws ExpressionException {
		Object o = exec(code, context);
		try {
			if (o instanceof Number) {
				return ((Number) o).intValue();
			}
		} catch (Exception ex) {
		}
		throw new ExpressionException("Could not convert " + o + " to int");
	}

	public static boolean execBoolean(String code, JexlContext context)
			throws ExpressionException {
		Object o = exec(code, context);
		try {
			if (o instanceof Boolean) {
				return ((Boolean) o).booleanValue();
			}
		} catch (Exception ex) {
		}
		throw new ExpressionException("Could not convert " + o + " to boolean");
	}

	public static double execDouble(String code, JexlContext context)
			throws ExpressionException {
		Object o = exec(code, context);
		try {
			if (o instanceof Number) {
				return ((Number) o).doubleValue();
			}
		} catch (Exception ex) {
		}
		throw new ExpressionException("Could not convert " + o + " to double");
	}

	public static String execString(String code, JexlContext context)
			throws ExpressionException {
		Object o = exec(code, context);
		if (o == null)
			return null;
		else
			return o.toString();
	}
}
