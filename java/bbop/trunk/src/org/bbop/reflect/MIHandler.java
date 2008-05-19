package org.bbop.reflect;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

import org.apache.log4j.*;

public class MIHandler implements InvocationHandler, MultipleInheritanceProxy {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(MIHandler.class);

	protected Map<Class, Object> map = new HashMap<Class, Object>();
	protected MultipleInheritanceProxy proxyObject;

	public MIHandler() {
		setDelegate(MultipleInheritanceProxy.class, this);
	}
	
	protected void setProxyObject(MultipleInheritanceProxy proxyObject) {
		this.proxyObject = proxyObject;
	}

	public Object invoke(Object proxy, Method method, Object[] args)
			throws Throwable {
		Object o = map.get(method.getDeclaringClass());
		if (o == null)
			throw new AbstractMethodError(
					"No delegate was set to handle the method " + method);
		return method.invoke(o, args);
	}

	public <T> T getDelegate(Class<T> theInterface) {
		return (T) map.get(theInterface);
	}

	public <T> void setDelegate(Class<T> theInterface, T object) {
		map.put(theInterface, object);
		if (object instanceof MultipleInheritanceDelegate) {
			((MultipleInheritanceDelegate) object).setProxy(this);
		}
	}

}
