package org.bbop.util;

import java.util.Comparator;

public class ComparableComparator implements Comparator {

	public static final ComparableComparator COMPARATOR = new ComparableComparator();
	
	public int compare(Object a, Object b) {
		Comparable ca = (Comparable) a;
		Comparable cb = (Comparable) b;
		return ca.compareTo(cb);
	}
}
