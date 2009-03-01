package org.geneontology.db.model;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 * The CVTerm class corresponds to the GO term table.  
 * @author Suzi Lewis
 *
 */
public class Term extends GOModel {

	/** The cvterm cterm_id */
	protected int term_id;

	protected String acc;

	/** The name of the cvterm */
	protected String name;

	/** The CV that the cvterm is part of */
	protected String cv;

	/** root of graph flag */
	protected Integer is_root;

	/** obsolete flag */
	protected Integer is_obsolete;

	protected TermDefinition term_definition;

	protected Set<Relationship> parents;

	protected Set<Relationship> children;

	/** The {@link DBXref} of the cvterm */
	protected Set<TermSynonym> synonyms;

	protected Set<TermDBXref> termDBXrefs;

	protected Set<Term> subsets;

	protected Set<MetaRelationship> considerations;

	/** 
	 * An association is a link between a gene product record and an ontology term, 
	 * with one or more pieces of evidence 
	 * *** IMPORTANT: NOT all associations are positive: some posit negative links.
	 */
	protected Set<Association> associations;

	protected String source;
	
	/** this may be general? maybe should be a real date - this is the time of the ontology
	   for files this would be the modification date, for loading from database somewhere
	  obd? zfin? this would be the time of the load */

	private long timestamp;

	public Term(){
		String[] uniqueConstraintFields = {"acc"};
		this.initUniqueConstraintFields(Term.class, uniqueConstraintFields);
	}

	/**
	 * Setter of CVTerm {@link CV}
	 */
	public void setCv(String cv) {
		this.cv = cv;
	}

	public String getCv() {
		return cv;
	}

	/**
	 * Getter of CVTerm is_root flag.
	 * @return 1 if root, 0 otherwise.
	 */
	public Integer getIs_root() {
		return is_root;
	}

	/**
	 * Setter of CVTerm is obsolete flag.
	 * @param is_obsolete obsolete flag.
	 */
	public void setIs_root(Integer is_root) {
		this.is_root = is_root;
	}

	public String getAcc() {
		return acc;
	}

	public void setAcc(String acc) {
		this.acc = acc;
	}

	public TermDefinition getTerm_definition() {
		return term_definition;
	}

	public void setTerm_definition(TermDefinition term_definition) {
		this.term_definition = term_definition;
	}

	/**
	 * Override the method in the parent class to support GO Style
	 * @return the definition of the cvterm.
	 */
	public String getDefinition() {
		return this.term_definition != null ? this.term_definition.getTerm_definition() : null;
	}

	/**
	 * Setter of the CVTerm definition.
	 * @param definition the definition of the cvterm.
	 */
	public void setDefinition(String definition) {
		if (this.term_definition == null) {
			this.term_definition = new TermDefinition();
		}
		this.term_definition.setTerm_id(term_id);
		this.term_definition.setTerm_definition(definition);
	}

	public int getTerm_id() {
		return term_id;
	}

	public void setTerm_id(int term_id) {
		this.term_id = term_id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Integer getIs_obsolete() {
		return is_obsolete;
	}

	public boolean isObsolete() {
		return is_obsolete != 0;
	}

	public void setIs_obsolete(Integer is_obsolete) {
		this.is_obsolete = is_obsolete;
	}

	public String toString() {
		return acc + " " + (name != null ? name : "");
	}

	public Set<TermSynonym> getSynonyms() {
		return synonyms;
	}

	public void setSynonyms(Set<TermSynonym> synonyms) {
		this.synonyms = synonyms;
	}

	/**
	 */
	public TermSynonym getSynonym() {
		return this.synonyms != null ? (TermSynonym) this.synonyms.iterator() : null;
	}

	/**
	 */
	public void setDbxref(TermSynonym synonym) {
		if (this.synonyms == null)
			this.synonyms = new HashSet<TermSynonym> (2);
		this.synonyms.add(synonym);
	}

	public Set<Relationship> getParents() {
		return parents;
	}

	public void setParents(Set<Relationship> parents) {
		this.parents = parents;
	}

	public Set<Relationship> getChildren() {
		return children;
	}

	public void setChildren(Set<Relationship> children) {
		this.children = children;
	}

	public Set<TermDBXref> getTermDBXrefs() {
		return termDBXrefs;
	}

	public void setTermDBXrefs(Set<TermDBXref> termDBXrefs) {
		this.termDBXrefs = termDBXrefs;
	}

	public Set<Term> getSubsets() {
		return subsets;
	}

	public void setSubsets(Set<Term> subsets) {
		this.subsets = subsets;
	}

	public Set<MetaRelationship> getConsiderations() {
		return considerations;
	}

	public void setConsiderations(Set<MetaRelationship> considerations) {
		this.considerations = considerations;
	}

	public Set<Association> getAssociations() {
		return associations;
	}

	public void setAssociations(Set<Association> associations) {
		this.associations = associations;
	}

	public boolean isParentOf(Term child) {
		if (getAcc().equals(child.getAcc()))
			return false;

		boolean is_child = false;
		for (Iterator<Relationship> it = children.iterator(); it.hasNext() && !is_child;) {
			Relationship relate = it.next();
			Term check = relate.getSubject();
			// System.out.println ("Comparing parent " + check.getName() + " to child " + child.getName());
			is_child = (check.getAcc().equals(child.getAcc()));
			if (!is_child) {
				is_child = check.isParentOf(child);
			}
		}
		return is_child;
	}
	
	public boolean isRoot() {
		return is_root.intValue() == 1;
	}

	public String getSource() {
		if (isRoot())
			return source;
		else
			return null;
	}

	public void setSource(String source) {
		if (isRoot())
			this.source = source;
	}

	public long getTimestamp() {
		if (isRoot())
			return timestamp;
		else
			return 0L;
	}

	public void setTimestamp(long timestamp) {
		if (isRoot())
			this.timestamp = timestamp;
	}

}