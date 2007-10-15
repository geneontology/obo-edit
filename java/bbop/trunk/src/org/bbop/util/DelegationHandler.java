package org.bbop.util;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

public class DelegationHandler implements InvocationHandler {

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
