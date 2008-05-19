package org.bbop.util;

import org.apache.log4j.*;

public class DefaultHasher implements Hasher {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DefaultHasher.class);

	protected static Hasher instance;
	
	public static Hasher getInstance() {
		if (instance == null) {
			instance = new DefaultHasher();
		}
		return instance;
	}
	
	public DefaultHasher() {
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
