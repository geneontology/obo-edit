package org.bbop.reflect;

public interface MultipleInheritanceProxy {

	public <T> void setDelegate(Class<T> theInterface, T object);
	public <T> T getDelegate(Class<T> theInterface);
}
