package org.bbop.util;

import java.util.*;

import org.apache.log4j.*;

public class SetUtil {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SetUtil.class);

    public static Set trueClone(Set set) {
	HashSet out = new HashSet();
	Iterator it = set.iterator();
	while(it.hasNext()) {
	    out.add(ObjectUtil.cloneObject(it.next()));
	}
	return out;
    }
}
