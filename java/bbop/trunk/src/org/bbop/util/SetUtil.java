package org.bbop.util;

import java.util.*;

public class SetUtil {

    public static Set trueClone(Set set) {
	HashSet out = new HashSet();
	Iterator it = set.iterator();
	while(it.hasNext()) {
	    out.add(ObjectUtil.cloneObject(it.next()));
	}
	return out;
    }
}
