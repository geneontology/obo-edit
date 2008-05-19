package org.bbop.expression;

import java.io.File;
import java.io.FileReader;
import java.io.RandomAccessFile;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

import org.bbop.expression.context.HashMapContext;

import org.apache.log4j.*;

public class Jexl {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(Jexl.class);
	
	public static class Temp {
		public int myVal = 99;

		private FunctionDef methodDelegate;
		
		public void setMethodDelegate(FunctionDef methodDelegate) {
			this.methodDelegate = methodDelegate;
		}
		
		public FunctionDef getMethodDelegate() {
			return methodDelegate;
		}

		public int getMyVal() {
			return myVal;
		}

		public void setMyVal(int myVal) {
			this.myVal = myVal;
		}
	}

	/**
	 * @param args
	 * @throws Exception 
	 */
	public static void main(String[] args) throws Exception {
		JexlContext context = new HashMapContext();
		Class[] classArgs = {Double.TYPE};
		Method m = null;
		try {
			m = Math.class.getMethod("abs", classArgs);
			context.defineGlobalFunction("abs", new FunctionMappingImpl(m));
			Class[] printArgs = {String.class};
			m = System.out.getClass().getMethod("print", printArgs);
			context.defineGlobalFunction("print", new FunctionMappingImpl(System.out, m));
			m = System.out.getClass().getMethod("println", printArgs);
			context.defineGlobalFunction("println", new FunctionMappingImpl(System.out, m));
			
		} catch (SecurityException e1) {
			e1.printStackTrace();
		} catch (NoSuchMethodException e1) {
			e1.printStackTrace();
		}		
		context.setGlobalVariable("temp", new Temp(), false);
		try {
			for (int i = 0; i < args.length; i++) {
				RandomAccessFile f = new RandomAccessFile(args[i], "r");
				byte [] arr = new byte[(int) f.length()];
				f.readFully(arr);
				f.close();
				String s = new String(arr);
				Script e = ScriptFactory.createScript(s);
				logger.info("Progam finished with return val "+e.execute(context.createNewFrame()));
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
