package org.obo.datamodel.impl;

import java.util.Collections;
import java.util.Date;
import java.util.Set;

import org.obo.datamodel.AnnotatedObject;
import org.obo.datamodel.Dbxref;
import org.obo.datamodel.NestedValue;
import org.obo.datamodel.OBOObject;
import org.obo.datamodel.ObsoletableObject;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.TermSubset;

public abstract class DanglingAnnotatedObjectImpl extends
		DanglingObjectImpl implements OBOObject {

	public DanglingAnnotatedObjectImpl(String id) {
		super(id);
	}

	public void addSecondaryID(String secondaryID) {

	}

	public void addSecondaryIDExtension(String id, NestedValue nv) {

	}

	public NestedValue getSecondaryIDExtension(String id) {

		return null;
	}

	public Set getSecondaryIDs() {
		return Collections.emptySet();
	}

	public void removeSecondaryID(String secondaryID) {
	}

	public void addSynonym(Synonym s) {
	}

	public Set<Synonym> getSynonyms() {
		return Collections.emptySet();
	}

	public void removeSynonym(Synonym s) {
	}

	public void addDbxref(Dbxref xref) {
	}

	public Set<Dbxref> getDbxrefs() {
		return Collections.emptySet();
	}

	public void removeDbxref(Dbxref xref) {
	}

	public String getComment() {
		return null;
	}

	public NestedValue getCommentExtension() {
		return null;
	}

	public void setComment(String comment) {
	}

	public void setCommentExtension(NestedValue nv) {
	}

	public void addConsiderExtension(ObsoletableObject o, NestedValue v) {
	}

	public void addConsiderReplacement(ObsoletableObject o) {
	}

	public void addReplacedBy(ObsoletableObject o) {
	}

	public void addReplacedByExtension(ObsoletableObject o, NestedValue v) {
	}

	public NestedValue getConsiderExtension(ObsoletableObject o) {
		return null;
	}

	public Set<ObsoletableObject> getConsiderReplacements() {
		return Collections.emptySet();
	}

	public NestedValue getObsoleteExtension() {
		return null;
	}

	public Set<ObsoletableObject> getReplacedBy() {
		return Collections.emptySet();
	}

	public NestedValue getReplacedByExtension(ObsoletableObject o) {
		return null;
	}

	public boolean isObsolete() {
		return false;
	}

	public void removeConsiderReplacement(ObsoletableObject o) {
	}

	public void removeReplacedBy(ObsoletableObject o) {
	}

	public void setObsolete(boolean isObsolete) {
	}

	public void setObsoleteExtension(NestedValue nv) {
	}

	public void addDefDbxref(Dbxref dbxref) {
	}

	public Set<Dbxref> getDefDbxrefs() {
		return Collections.emptySet();
	}

	public String getDefinition() {
		return null;
	}

	public NestedValue getDefinitionExtension() {
		return null;
	}

	public void removeDefDbxref(Dbxref dbxref) {
	}

	public void setDefinition(String definition) {
	}

	public void setDefinitionExtension(NestedValue nv) {
	}

	public Date getCreationDate() {
		return null;
	}

	public String getCreatedBy() {
		return null;
	}

	public void setCreationDate(Date date) {
	}

	public void setCreatedBy(String name) {
	}

	public String getModifiedBy() {
		return null;
	}

	public void setModifiedBy(String username) {
	}

	public Date getModificationDate() {
		return null;
	}

	public void setModificationDate(Date date) {
	}

	public NestedValue getCreationDateExtension() {
		return null;
	}

	public void setCreationDateExtension(NestedValue creationDateExtension) {
	}

	public NestedValue getModificationDateExtension() {
		return null;
	}

	public void setModificationDateExtension(
			NestedValue modificationDateExtension) {
	}

	public NestedValue getModifiedByExtension() {
		return null;
	}

	public void setModifiedByExtension(NestedValue modifiedByExtension) {
	}

	public NestedValue getCreatedByExtension() {
		return null;
	}

	public void setCreatedByExtension(NestedValue createdByExtension) {
	}
	

	public void addCategory(TermSubset category) {
	}

	public void addCategoryExtension(TermSubset category, NestedValue nv) {
	}

	public Set<TermSubset> getSubsets() {
		return Collections.emptySet();
	}

	public NestedValue getCategoryExtension(TermSubset category) {
		return null;
	}

	public void removeCategory(TermSubset category) {
	}

}
