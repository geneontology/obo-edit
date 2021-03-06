package org.obo.datamodel.impl;

import java.util.Set;

import org.obo.datamodel.DanglingProperty;
import org.obo.datamodel.Dbxref;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.NestedValue;
import org.obo.datamodel.ObsoletableObject;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.TermCategory;
import org.obo.datamodel.Type;

public class DanglingPropertyImpl extends DanglingAnnotatedObjectImpl implements
		DanglingProperty {

	public DanglingPropertyImpl(String id) {
		super(id);
	}
	
	public boolean isNonInheritable() {
		return false;
	}

	public NestedValue getCyclicExtension() {
		return null;
	}

	public IdentifiedObject getDomain() {
		return null;
	}

	public NestedValue getDomainExtension() {
		return null;
	}

	public Type getRange() {
		return null;
	}

	public NestedValue getRangeExtension() {
		return null;
	}

	public NestedValue getReflexiveExtension() {
		return null;
	}

	public NestedValue getSymmetricExtension() {
		return null;
	}

	public NestedValue getTransitiveExtension() {
		return null;
	}
	
	public boolean isAlwaysImpliesInverse() {
		return false;
	}
	
	public void setAlwaysImpliesInverse(boolean inverse) {
	}

	public boolean isCyclic() {
		return false;
	}

	public boolean isReflexive() {
		return false;
	}

	public boolean isSymmetric() {
		return false;
	}

	public boolean isTransitive() {
		return false;
	}
	
	public boolean isDummy() {
		return false;
	}

	public void setCyclic(boolean cyclic) {
		
	}

	public void setCyclicExtension(NestedValue nv) {
	}

	public void setDomain(IdentifiedObject domain) {
	}

	public void setDomainExtension(NestedValue domainExtension) {
	}

	public void setRange(Type range) {
	}

	public void setRangeExtension(NestedValue domainExtension) {
	}

	public void setReflexiveExtension(NestedValue nv) {
	}

	public void setSymmetric(boolean symmetric) {
	}

	public void setSymmetricExtension(NestedValue nv) {
	}

	public void setTransitive(boolean transitive) {
	}

	public void setTransitiveExtension(NestedValue nv) {}

	public void addCategory(TermCategory category) {
	}

	public void addCategoryExtension(TermCategory category, NestedValue nv) {
	}

	public Set<TermCategory> getCategories() {
		return null;
	}

	public NestedValue getCategoryExtension(TermCategory category) {
		return null;
	}

	public void removeCategory(TermCategory category) {
	}

	public NestedValue getAlwaysImpliesInverseExtension() {
		return null;
	}
	
	public void setReflexive(boolean reflexive) {
	}

	public void setAlwaysImpliesInverseExtension(NestedValue nv) {
	}

}
