package org.geneontology.db.model;

import java.util.Vector;

/**
 * The CVTerm class corresponds to the Chado cvterm table.  
 * @author Robert Bruggner
 *
 */
public class Term extends GOModel {
		
	/** The cvterm cterm_id */
	protected int go_term_id;
	
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
	
	/** The {@link DBXref} of the cvterm */
	protected Vector<DBXref> dbxref;

	public Term(){
		String[] uniqueConstraintFields = {"acc"};
		this.initUniqueConstraintFields(Term.class,uniqueConstraintFields);
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
		this.term_definition.setTerm_id(go_term_id);
		this.term_definition.setTerm_definition(definition);
	}

	/**
	 * Override Getter of the CVTerm {@link DBXref} object.
	 * @return cvterm {@link DBXref} object.
	 */
	public DBXref getDbxref() {
		return this.dbxref != null ? (DBXref) this.dbxref.get(0) : null;
}

	/**
	 * Override Setter of the CVTerm {@link DBXref} object.
	 * @param dbxref the {@link DBXref} object of the cvterm.
	 */
	public void setDbxref(DBXref dbxref) {
		if (this.dbxref == null)
			this.dbxref = new Vector<DBXref> (2);
		this.dbxref.add(dbxref);
	}

	public int getGo_term_id() {
		return go_term_id;
	}

	public void setGo_term_id(int go_term_id) {
		this.go_term_id = go_term_id;
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

	public void setIs_obsolete(Integer is_obsolete) {
		this.is_obsolete = is_obsolete;
	}
	

}