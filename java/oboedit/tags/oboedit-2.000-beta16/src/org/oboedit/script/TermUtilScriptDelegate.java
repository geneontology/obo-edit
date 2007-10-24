package org.oboedit.script;

import java.util.Collection;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.LinkedObject;
import org.obo.util.TermUtil;

public class TermUtilScriptDelegate {
	public static boolean isProperty(IdentifiedObject io) {
		return TermUtil.isProperty(io);
	}
	
	public static boolean isObsolete(IdentifiedObject io) {
		return TermUtil.isObsolete(io);
	}
	
	public static boolean isIntersection(IdentifiedObject io) {
		if (io instanceof LinkedObject)
			return TermUtil.isIntersection((LinkedObject) io);
		else
			return false;
	}
	
	public static boolean isTerm(IdentifiedObject io) {
		return TermUtil.isClass(io);
	}
	
	public static LinkedObject getRoot(LinkedObject lo) {
		return TermUtil.getRoot(lo);
	}
	
	public static Collection getDescendants(LinkedObject lo,
			boolean includeSelf) {
		return TermUtil.getDescendants(lo, includeSelf);
	}
}
