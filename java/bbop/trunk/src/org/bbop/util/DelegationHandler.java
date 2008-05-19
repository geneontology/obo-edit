package org.bbop.util;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

import org.apache.log4j.*;

public class DelegationHandler implements InvocationHandler {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DelegationHandler.class);

	protected Object target;

	public DelegationHandler(Object target) {
		this.target = target;
	}

	public Object invoke(Object proxy, Method method, Object[] args)
			throws Throwable {
		return method.invoke(target, args);
	}

	public static <T> T getDelegate(Object in, DelegationHandler handler,
			Class<T> outputType, Class... interfaces) {
		Object o = Proxy.newProxyInstance(in.getClass().getClassLoader(),
				interfaces, handler);
		return (T) o;
	}
}
