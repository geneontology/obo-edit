package org.bbop.util;

import java.lang.reflect.*;

public class ObjectUtil {

    public static boolean equals(Object a, Object b) {
	// if they're both null, they're equal
	if (a == null && b == null)
	    return true;
	// if either is null but not both, they're not equal
	if (a == null || b == null)
	    return false;
	return (a.equals(b));
    }

    public static Object cloneObject(Object in) {
	try {
	    Method cloneMethod = in.getClass().
		getDeclaredMethod("clone", new Class[0]);
	    Object clone = cloneMethod.invoke(in, new Object[0]);
	    return clone;
	} catch (Exception e) {
	    return null;
	}
    }
}
