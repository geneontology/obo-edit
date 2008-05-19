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
import org.obo.datamodel.OBOClass;
import org.obo.query.Query;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class NamespaceQuery implements Query<OBOClass, OBOClass> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(NamespaceQuery.class);

	protected Collection<String> namespaceIDs = new HashSet<String>();

	protected boolean allowObsoletes = true;

	protected boolean allowNonObsoletes = true;

	protected Comparator<OBOClass> comparator = new Comparator<OBOClass>() {
		public int compare(OBOClass o1, OBOClass o2) {
			return o1.getName().compareToIgnoreCase(o2.getName());
		}
	};

	public NamespaceQuery() {

	}

	public NamespaceQuery(Namespace... namespaces) {
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

	public NamespaceQuery(String... namespaceIDs) {
		for (String ns : namespaceIDs)
			this.namespaceIDs.add(ns);
	}

	public NamespaceQuery(Collection<String> namespaceIDs,
			boolean allowObsoletes, boolean allowNonObsoletes) {
		this.namespaceIDs = namespaceIDs;
		setAllowNonObsoletes(allowNonObsoletes);
		setAllowObsoletes(allowObsoletes);
	}

	public void setAllowNonObsoletes(boolean allowNonObsoletes) {
		this.allowNonObsoletes = allowNonObsoletes;
	}

	public NamespaceQuery(Collection<String> namespaceIDs) {
		this(namespaceIDs, true, true);
	}

	public OBOClass convertToInputType(OBOClass original) {
		return original;
	}

	public Collection<OBOClass> createResultHolder() {
		return new ArrayList<OBOClass>();
	}

	public void setComparator(Comparator<OBOClass> comparator) {
		this.comparator = comparator;
	}

	public Comparator<OBOClass> getComparator() {
		return comparator;
	}

	public Class<OBOClass> getInputType() {
		return OBOClass.class;
	}

	public OBOClass matches(OBOClass a) {
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
		if (o instanceof NamespaceQuery) {
			NamespaceQuery nq = (NamespaceQuery) o;
			return ObjectUtil.equals(nq.getNamespaceIDs(),
						namespaceIDs) && (getAllowObsoletes() == nq.getAllowObsoletes()) &&
						(getAllowNonObsoletes() == nq.getAllowNonObsoletes());
		} else
			return false;
	}

	public Collection<String> getNamespaceIDs() {
		return namespaceIDs;
	}

	public OBOClass convertToOutputType(OBOClass original) {
		return original;
	}

	public Collection<FieldPathSpec> getInputPaths() {
		return null;
	}

	public void setFieldPath(FieldPath path) {
	}
}
