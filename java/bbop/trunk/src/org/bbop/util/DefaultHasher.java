package org.bbop.util;

public class DefaultHasher implements Hasher {

	protected static Hasher instance;
	
	public static Hasher getInstance() {
		if (instance == null) {
			instance = new DefaultHasher();
		}
		return instance;
	}
	
	public boolean equals(Object a, Object b) {
		return ObjectUtil.equals(a, b);
	}

	public String hashCode(Object o) {
		if (o == null)
			return null;
		else
			return ""+o.hashCode();
	}

}
