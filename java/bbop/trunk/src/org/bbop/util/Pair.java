package org.bbop.util;

import org.apache.log4j.*;

public class Pair {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(Pair.class);
    public Object a;
    public Object b;

    public Pair(Object a, Object b) {
	this.a = a;
	this.b = b;
    }

    public Object getA() {
	return a;
    }

    public Object getB() {
	return b;
    }
}
