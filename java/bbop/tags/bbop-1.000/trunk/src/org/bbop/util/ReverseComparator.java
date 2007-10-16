package org.bbop.util;

import java.util.Comparator;

/**
 * A ReverseComparator wraps another Comparator and returns the reverse
 * ordering of the wrapped comparator.
 */
public class ReverseComparator implements Comparator {

    private Comparator original;
    
    public ReverseComparator(Comparator in) {
	original = in;
    }
    
    public int compare(Object a, Object b) {
	int comp = original.compare(a, b);
	if (comp < 0)
	    return 1;
	else if (comp > 0)
	    return -1;
	else
	    return 0;
    }
}

