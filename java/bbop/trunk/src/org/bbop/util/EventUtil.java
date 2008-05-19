package org.bbop.util;

import java.awt.EventQueue;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

import javax.swing.SwingUtilities;

import org.apache.log4j.*;

public class EventUtil {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(EventUtil.class);

	private EventUtil() {
	}

	protected static abstract class ReturnValRunnable implements Runnable {
		protected Object val;
		protected Throwable exception;
		
		public void setException(Throwable exception) {
			this.exception = exception;
		}
		
		public Throwable getException() {
			return exception;
		}

		public void setVal(Object val) {
			this.val = val;
		}

		public Object getVal() {
			return val;
		}
	}

	protected static class AWTSafeDelegate implements InvocationHandler {

		protected Object target;

		public AWTSafeDelegate(Object target) {
			this.target = target;
		}

		public Object invoke(Object proxy, final Method method,
				final Object[] args) throws Throwable {
			if (SwingUtilities.isEventDispatchThread() || isObjectMethod(method)) {
				return method.invoke(target, args);
			}
			ReturnValRunnable r = new ReturnValRunnable() {
				public void run() {
					try {
						setVal(method.invoke(target, args));
					} catch (Throwable t) {
						setException(t);
					}
				}
			};
			if (method.getReturnType().equals(Void.TYPE)) {
				SwingUtilities.invokeLater(r);
				return null;
			} else {
				SwingUtilities.invokeAndWait(r);
				if (r.getException() != null)
					throw r.getException();
				else
					return r.getVal();
			}
		}

	}
	
	protected static boolean isObjectMethod(Method m) {
		return m.getDeclaringClass().equals(Object.class);
	}

	public static AsynchronousListener getThreadSafeListener(
			AsynchronousListener l) {
		return (AsynchronousListener) Proxy.newProxyInstance(l.getClass()
				.getClassLoader(), l.getClass().getInterfaces(),
				new AWTSafeDelegate(l));
	}
}
