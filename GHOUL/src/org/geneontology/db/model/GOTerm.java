package org.geneontology.db.model;

import java.util.Vector;

import org.gmod.db.chAPI.simpleObject.CV;
import org.gmod.db.chAPI.simpleObject.CVTerm;
import org.gmod.db.chAPI.simpleObject.DBXref;

/**
 * The CVTerm class corresponds to the Chado cvterm table.  
 * @author Robert Bruggner
 *
 */
public class GOTerm extends CVTerm {
		
	/** root of graph flag */
	protected Integer is_root;
	
	protected TermDefinition term_definition;
	
	protected String acc;

	/** The {@link DBXref} of the cvterm */
	protected Vector dbxrefs;

	public GOTerm(){
		String[] uniqueConstraintFields = {"acc"};
		this.initUniqueConstraintFields(GOTerm.class,uniqueConstraintFields);
	}

	/**
	 * Setter of CVTerm {@link CV}
	 * @param cv the {@link CV} that this CVTerm is part of.
	 */
	public void setCv(String cv) {
		this.cv = new CV();
		this.cv.setName(cv);
	}
	
	/**
	 * Getter of CVTerm name.
	 * @return the cvterm name.
	 */
	public String getTerm_type() {
		return cv != null ? cv.getName() : null;
	}
	
	/**
	 * Setter of CVTerm name.
	 * @param name the cvterm name.
	 */
	public void setTerm_type(String name) {
		cv = new CV();
		cv.setName(name);
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
		this.term_definition.setTerm_id(cvterm_id);
		this.term_definition.setTerm_definition(definition);
		}
	}

	/**
	 * Override Getter of the CVTerm {@link DBXref} object.
	 * @return cvterm {@link DBXref} object.
	 */
	public DBXref getDbxref() {
		return dbxrefs != null ? (DBXref) dbxrefs.get(0) : null;
}

	/**
	 * Override Setter of the CVTerm {@link DBXref} object.
	 * @param dbxref the {@link DBXref} object of the cvterm.
	 */
	public void setDbxref(DBXref dbxref) {
		if (dbxrefs == null)
			dbxrefs = new Vector (2);
		this.dbxrefs.add(dbxref);
	}

}