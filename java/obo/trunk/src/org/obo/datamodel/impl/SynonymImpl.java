package org.obo.datamodel.impl;

import java.util.*;

import org.bbop.util.*;
import org.obo.datamodel.*;

import org.apache.log4j.*;

public class SynonymImpl implements Synonym {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SynonymImpl.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 45485650012520403L;
	protected String text;
	protected Set references;
	protected int type;
	protected SynonymCategory category;
	protected String privateid;

	private static int idgen = 0;
	protected NestedValue nestedValue;

	public SynonymImpl() {
		this(null);
	}

	public SynonymImpl(String text) {
		this(text, RELATED_SYNONYM);
	}

	public SynonymImpl(String text, int type) {
		setScope(type);
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

	public SynonymCategory getSynonymCategory() {
		return category;
	}

	public void setSynonymCategory(SynonymCategory category) {
		this.category = category;
	}

	public void setNestedValue(NestedValue nestedValue) {
		this.nestedValue = nestedValue;
	}

	public NestedValue getNestedValue() {
		return nestedValue;
	}

	public int getScope() {
		if (category == null || category.getScope() == UNKNOWN_SCOPE)
			return type;
		else
			return category.getScope();
	}

	public void setScope(int type) {
		if (type == UNKNOWN_SCOPE)
			throw new IllegalArgumentException("Cannot set synonym scope to "
					+ "UNKNOWN_SCOPE");
		this.type = type;
	}

	public void addDbxref(Dbxref ref) {
		if (!references.contains(ref))
			references.add(ref);
	}

	public void removeDbxref(Dbxref ref) {
		references.remove(ref);
	}

	public Set getDbxrefs() {
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
				s.addDbxref((Dbxref) ref.clone());
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
					&& ObjectUtil.equals(category, in.getSynonymCategory())
					&& type == in.getScope()
					&& in.getDbxrefs().equals(references);
		} else
			return false;
	}
}
