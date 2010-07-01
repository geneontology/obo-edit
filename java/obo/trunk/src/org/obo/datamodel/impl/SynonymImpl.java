package org.obo.datamodel.impl;

//import java.util.*;
import java.util.Iterator;
import java.util.Set;
import java.util.HashSet;

import org.bbop.util.*;
import org.obo.datamodel.*;

public class SynonymImpl implements Synonym {
	private static final long serialVersionUID = 45485650012520403L;
	protected String text;
	protected Set references;
	protected int scope;
	protected SynonymType syntype;
	protected String privateid;

	private static int idgen = 0;
	protected NestedValue nestedValue;

	public SynonymImpl() {
		this(null);
	}

	public SynonymImpl(String text) {
		this(text, RELATED_SYNONYM);
	}

	public SynonymImpl(String text, int scope) {
		setScope(scope);
		this.text = text;
		privateid = ""+idgen++;
		references = new HashSet();
	}
	
	public SynonymImpl(String text, int scope, SynonymType syntype) {
		setSynonymType(syntype);
		setScope(scope);
		this.text = text;
		privateid = ""+idgen++;
		references = new HashSet();
	}
	
	public String getID() {
		return privateid;
	}
	
	public boolean isAnonymous() {
		return true;
	}

	public SynonymType getSynonymType() {
		return syntype;
	}

	public void setSynonymType(SynonymType syntype) {
		this.syntype = syntype;
	}

	public void setNestedValue(NestedValue nestedValue) {
		this.nestedValue = nestedValue;
	}

	public NestedValue getNestedValue() {
		return nestedValue;
	}

	public int getScope() {
		if (syntype == null || syntype.getScope() == UNKNOWN_SCOPE)
			return scope;
		else
			return syntype.getScope();
	}

	public void setScope(int scope) {
		if (scope == UNKNOWN_SCOPE)
			throw new IllegalArgumentException("Cannot set synonym scope to "
					+ "UNKNOWN_SCOPE");
		this.scope = scope;
	}

	public void addXref(Dbxref ref) {
		if (!references.contains(ref))
			references.add(ref);
	}

	public void removeXref(Dbxref ref) {
		references.remove(ref);
	}

	public Set getXrefs() {
		return references;
	}

	public void setDbxrefs(Set references) {
		this.references = references;
	}

	@Override
	public Object clone() {
		try {
			Synonym s = (Synonym) super.clone();
			((SynonymImpl) s).references = new HashSet();
			Iterator it = references.iterator();
			while (it.hasNext()) {
				Dbxref ref = (Dbxref) it.next();
				s.addXref((Dbxref) ref.clone());
			}

			return s;
		} catch (CloneNotSupportedException e) {
			// will never happen
			return null;
		}
	}

	@Override
	public String toString() {
		return text;
	}

	public String getText() {
		return text;
	}

	public void setText(String text) {
		this.text = text;
	}

	public int compareTo(Object o) {
		return COMPARATOR.compare(this, o);
	}

	@Override
	public boolean equals(Object o) {
		if (o instanceof Synonym) {
			Synonym in = (Synonym) o;
			return ObjectUtil.equals(text, in.getText())
					&& ObjectUtil.equals(syntype, in.getSynonymType())
					&& scope == in.getScope()
					&& in.getXrefs().equals(references);
		} else
			return false;
	}
}
