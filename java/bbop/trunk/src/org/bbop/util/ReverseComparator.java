package org.bbop.util;

import java.util.Comparator;

/**
 * A ReverseComparator wraps another Comparator and returns the reverse
 * ordering of the wrapped comparator.
 */
import org.apache.log4j.*;

public class ReverseComparator implements Comparator {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ReverseComparator.class);

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

