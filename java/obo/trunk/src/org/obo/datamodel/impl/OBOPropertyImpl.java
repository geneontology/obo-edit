package org.obo.datamodel.impl;

import org.obo.datamodel.*;

import java.util.*;

import org.apache.log4j.*;

public class OBOPropertyImpl extends LinkedAnnotatedObjectImpl implements
	OBOProperty {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OBOPropertyImpl.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1781810097216245117L;
	protected Type range;
	protected IdentifiedObject domain;
	protected NestedValue domainExtension;
	protected NestedValue rangeExtension;

	protected NestedValue cyclicExtension;
	protected NestedValue symmetricExtension;
	protected NestedValue transitiveExtension;
	protected NestedValue reflexiveExtension;
	protected NestedValue alwaysImpliesInverseExtension;

	protected boolean cyclic;
	protected boolean symmetric;
	protected boolean transitive;
	protected boolean reflexive;
	protected boolean alwaysImpliesInverse;
	protected boolean universallyQuantified;
	
	protected OBOProperty transitiveOver;

	protected Set categories;
	protected HashMap categoryExtensionHash;

	@Override
	public void addCategoryExtension(TermCategory category, NestedValue nv) {
		if (categoryExtensionHash == null)
			categoryExtensionHash = new HashMap();
		categoryExtensionHash.put(category, nv);
	}

	@Override
	public NestedValue getCategoryExtension(TermCategory category) {
		if (categoryExtensionHash == null)
			return null;
		return (NestedValue) categoryExtensionHash.get(category);
	}

	@Override
	public void addCategory(TermCategory category) {
		categories.add(category);
	}

	@Override
	public void removeCategory(TermCategory category) {
		categories.remove(category);
	}
	
	public boolean isNonInheritable() {
		return false;
	}

	@Override
	public Set getCategories() {
		return categories;
	}

	public OBOPropertyImpl(String id, String name) {
		super(id);
		this.name = name;
		categories = new HashSet();
	}

	public OBOPropertyImpl(String id) {
		super(id);
		// CJM 2008-05-07 : leave name as null if not set
		//this.name = id;
		categories = new HashSet();
	}

	@Override
	public String toString() {
		if (id.equals(name))
			return id;
		else
			return super.toString();
	}

	public void setCyclic(boolean cyclic) {
		this.cyclic = cyclic;
	}

	public void setSymmetric(boolean symmetric) {
		this.symmetric = symmetric;
	}

	public void setTransitive(boolean transitive) {
		this.transitive = transitive;
	}

	public void setReflexive(boolean reflexive) {
		this.reflexive = reflexive;
	}

	public boolean isReflexive() {
		return reflexive;
	}

	public boolean isTransitive() {
		return transitive;
	}

	public boolean isSymmetric() {
		return symmetric;
	}

	public boolean isCyclic() {
		return cyclic;
	}

	public Type getType() {
		return OBOClass.OBO_PROPERTY;
	}

	public NestedValue getCyclicExtension() {
		return cyclicExtension;
	}

	public NestedValue getSymmetricExtension() {
		return symmetricExtension;
	}

	public NestedValue getTransitiveExtension() {
		return transitiveExtension;
	}

	public NestedValue getReflexiveExtension() {
		return reflexiveExtension;
	}

	public void setReflexiveExtension(NestedValue nv) {
		this.reflexiveExtension = nv;
	}

	public void setCyclicExtension(NestedValue nv) {
		this.cyclicExtension = nv;
	}

	public void setSymmetricExtension(NestedValue nv) {
		this.symmetricExtension = nv;
	}

	public void setTransitiveExtension(NestedValue nv) {
		this.transitiveExtension = nv;
	}

	public NestedValue getDomainExtension() {
		return domainExtension;
	}

	public NestedValue getRangeExtension() {
		return rangeExtension;
	}

	public void setRangeExtension(NestedValue rangeExtension) {
		this.rangeExtension = rangeExtension;
	}

	public void setDomainExtension(NestedValue domainExtension) {
		this.domainExtension = domainExtension;
	}

	public void setRange(Type range) {
		this.range = range;
	}

	public void setDomain(IdentifiedObject domain) {
		this.domain = domain;
	}

	public Type getRange() {
		return range;
	}

	public IdentifiedObject getDomain() {
		return domain;
	}

	public NestedValue getAlwaysImpliesInverseExtension() {
		return alwaysImpliesInverseExtension;
	}

	public void setAlwaysImpliesInverseExtension(
			NestedValue alwaysImpliesInverseExtension) {
		this.alwaysImpliesInverseExtension = alwaysImpliesInverseExtension;
	}

	public boolean isAlwaysImpliesInverse() {
		return alwaysImpliesInverse;
	}

	public void setAlwaysImpliesInverse(boolean alwaysImpliesInverse) {
		this.alwaysImpliesInverse = alwaysImpliesInverse;
	}
	
	public boolean isDummy() {
		return false;
	}

	public OBOProperty getTransitiveOver() {
		//return transitiveOver;
		for (Link link : this.getParents()) {
			if (link.getType().getID().equals("transitive_over")) {
				return (OBOProperty)link.getParent();
			}
		}
		return null;
	}

	public void setTransitiveOver(OBOProperty transitiveOver) {
		OBORestrictionImpl link = new OBORestrictionImpl();
		link.setChild(this);
		link.setType(OBOProperty.TRANSITIVE_OVER);
		link.setParent(transitiveOver);
		addParent(link);
	}

	public boolean isUniversallyQuantified() {
		return universallyQuantified;
	}

	public void setUniversallyQuantified(boolean universallyQuantified) {
		this.universallyQuantified = universallyQuantified;
	}
	

}
