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
    with namespaces. A Collection of OBOObjects are returned from the query */
public class NamespaceObjQuery implements Query<OBOObject, OBOObject> {

	protected Collection<String> namespaceIDs = new HashSet<String>();

	protected boolean allowObsoletes = true;

	protected boolean allowNonObsoletes = true;

	protected Comparator<OBOObject> comparator = new Comparator<OBOObject>() {
		public int compare(OBOObject o1, OBOObject o2) {
			return o1.getName().compareToIgnoreCase(o2.getName());
		}
	};

	public NamespaceObjQuery() {

	}

	public NamespaceObjQuery(Namespace... namespaces) {
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

	public OBOObject matches(OBOObject a) {
		boolean isObsolete = TermUtil.isObsolete(a);
		if (!a.isBuiltIn()
				&& TermUtil.isClass(a)
				&& (namespaceIDs.size() == 0 || namespaceIDs.contains(a
						.getNamespace().getID()))
				&& ((!isObsolete && allowNonObsoletes) || (isObsolete && allowObsoletes))) {
			return a;
		} else
			return null;
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
