package org.obo.query.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedHashSet;

import org.bbop.util.ObjectUtil;
import org.obo.datamodel.FieldPath;
import org.obo.datamodel.FieldPathSpec;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.OBOObject;
import org.obo.query.Query;
import org.obo.util.TermUtil;

/** this is identical to NamespaceQuery except that it works on OBOObjects instead
    of OBOClasses, so its more general as OBOClass is an OBOObject, and so is
    OBOProperty. NamespaceObjQuery allows for the querying of obo sessions
    with namespaces. A Collection of OBOObjects are returned from the query
    The most important method here is the matches method as that is the really 
    the heart of the query/filter.
 */
public class NamespaceObjQuery implements Query<OBOObject, OBOObject> {

	protected Collection<String> namespaceIDs = new HashSet<String>();

	protected boolean allowObsoletes = true;

	protected boolean allowNonObsoletes = true;

  /** if justTerms true do just OBOClass, false just OBOProperties */
  private boolean justTerms = true;

	protected Comparator<OBOObject> comparator = new Comparator<OBOObject>() {
		public int compare(OBOObject o1, OBOObject o2) {
      if (o1.getName()==null && o2.getName()==null) return 0;
      if (o1.getName()==null) return 1;
      if (o2.getName()==null) return -1;
			return o1.getName().compareToIgnoreCase(o2.getName());
		}
	};

	public NamespaceObjQuery() {

	}

  /** if just terms true do just OBOClass, false just OBOProperties */
	public NamespaceObjQuery(boolean justTerms, Namespace... namespaces) {
    this.justTerms = justTerms;
		for (Namespace ns : namespaces) {
			namespaceIDs.add(ns.getID());
		}
	}

	public void setAllowObsoletes(boolean allowObsoletes) {
		this.allowObsoletes = allowObsoletes;
	}
	
	public boolean getAllowObsoletes() {
		return allowObsoletes;
	}
	
	public boolean getAllowNonObsoletes() {
		return allowNonObsoletes;
	}

	public NamespaceObjQuery(String... namespaceIDs) {
		for (String ns : namespaceIDs)
			this.namespaceIDs.add(ns);
	}

	public NamespaceObjQuery(Collection<String> namespaceIDs,
			boolean allowObsoletes, boolean allowNonObsoletes) {
		this.namespaceIDs = namespaceIDs;
		setAllowNonObsoletes(allowNonObsoletes);
		setAllowObsoletes(allowObsoletes);
	}

	public void setAllowNonObsoletes(boolean allowNonObsoletes) {
		this.allowNonObsoletes = allowNonObsoletes;
	}

	public NamespaceObjQuery(Collection<String> namespaceIDs) {
		this(namespaceIDs, true, true);
	}

	public OBOObject convertToInputType(OBOObject original) {
		return original;
	}

	public Collection<OBOObject> createResultHolder() {
		return new ArrayList<OBOObject>();
	}

	public void setComparator(Comparator<OBOObject> comparator) {
		this.comparator = comparator;
	}

	public Comparator<OBOObject> getComparator() {
		return comparator;
	}

	public Class<OBOObject> getInputType() {
		return OBOObject.class;
	}

  /** This is the method that is utlimately called by the Query machinery to say
      whether an OBOObject passes or not, returns null on failure, returns the
      OBOObject passed in on success */
	public OBOObject matches(OBOObject a) {
    // a builtIn is one of the building block obo terms, like obo:term
    if (a.isBuiltIn()) return null;
    if (!typeCheck(a)) return null;
    if (!namespaceMatch(a)) return null;
    if (!obsoleteCheck(a)) return null;
    return a;
// 		//boolean isObsolete = TermUtil.isObsolete(a);
// 		if (!a.isBuiltIn()
// 				// && TermUtil.isClass(a) this can work for properties as well as classes!
// 				&& (namespaceIDs.size() == 0 || namespaceIDs.contains(a
// 						.getNamespace().getID()))
// 				&& ((!isObsolete && allowNonObsoletes) || (isObsolete && allowObsoletes))) {
// 			return a;
// 		} else
// 			return null;
	}

  /** return true if obj id in namespaceIDs, or if namespaceIDs are empty */
  private boolean namespaceMatch(OBOObject obj) {
    if (namespaceIDs.isEmpty()) return true; // no namespaces
    if (obj.getNamespace() == null) {
      // odd - this wont print - think stdout is being taken somewhere
      //System.err.println("OBOObject with null namespace "+obj.getName());
      return false;
    }
    return namespaceIDs.contains(obj.getNamespace().getID());
  }
  /** returns true if is not obsolete and allowNonObsolete, or vice versa */
  private boolean obsoleteCheck(OBOObject obj) {
		boolean isObsolete = TermUtil.isObsolete(obj);
    if (!isObsolete && allowNonObsoletes) return true;
    return isObsolete && allowObsoletes;
  }
  /** returns true if is term/OBOClass and justTerms is true 
      OR if is OBOProperty and justTerms is false */
  private boolean typeCheck(OBOObject obj) {
    if (TermUtil.isClass(obj) && justTerms) return true;
    return TermUtil.isProperty(obj) && !justTerms;
  }

	public boolean equals(Object o) {
		if (o instanceof NamespaceObjQuery) {
			NamespaceObjQuery nq = (NamespaceObjQuery) o;
			return ObjectUtil.equals(nq.getNamespaceIDs(),
						namespaceIDs) && (getAllowObsoletes() == nq.getAllowObsoletes()) &&
						(getAllowNonObsoletes() == nq.getAllowNonObsoletes());
		} else
			return false;
	}

	public Collection<String> getNamespaceIDs() {
		return namespaceIDs;
	}

	public OBOObject convertToOutputType(OBOObject original) {
		return original;
	}

	public Collection<FieldPathSpec> getInputPaths() {
		return null;
	}

	public void setFieldPath(FieldPath path) {
	}
}
