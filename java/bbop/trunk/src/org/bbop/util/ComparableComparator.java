package org.bbop.util;

import java.util.Comparator;

import org.apache.log4j.*;

public class ComparableComparator implements Comparator {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ComparableComparator.class);

	public static final ComparableComparator COMPARATOR = new ComparableComparator();
	
	public int compare(Object a, Object b) {
		Comparable ca = (Comparable) a;
		Comparable cb = (Comparable) b;
		return ca.compareTo(cb);
	}
}
