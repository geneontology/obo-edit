package org.bbop.util;

import org.apache.log4j.*;

public class EqualsEqualityComparator implements EqualityComparator {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(EqualsEqualityComparator.class);
    public boolean equals(Object a, Object b) {
	return ObjectUtil.equals(a, b);
    }
}
