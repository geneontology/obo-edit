package org.geneontology.db.model;

import org.gmod.db.chAPI.simpleObject.DB;

/**
 * The DB class corresponds to the Chado db table.
 * @author Robert Bruggner
 *
 */
public class GOXrefDB extends DB {
	
	/** The full name of the DB */
	protected String fullname;

	/** An example URL of the DB */
	protected String url_example;

	/** The URL syntax for the DB */
	protected String url_syntax;

	public GOXrefDB(){
		String[] uniqueConstraintFields = {"name"};
		this.initUniqueConstraintFields(DB.class,uniqueConstraintFields);
	}

	/**
	 * Getter of DB name.
	 * @return name of DB.
	 */
	public String getFullname() {
		return fullname;
	}

	/**
	 * Setter of DB name.
	 * @param name name of DB.
	 */
	public void setFullname(String fullname) {
		this.fullname = fullname;
	}

	/**
	 * Getter of DB url
	 * @return URL of DB.
	 */
	public String getUrl_example() {
		return url_example;
	}

	/**
	 * Setter of DB url.
	 * @param url URL of DB.
	 */
	public void setUrl_example(String url_example) {
		this.url_example = url_example;
	}

	/**
	 * Getter of DB URL Prefix 
	 * @return URL Prefix of DB.
	 */
	public String getUrl_syntax() {
		return url_syntax;
	}

	/**
	 * Setter of DB URL Prefix 
	 * @param urlprefix DB URL Prefix
	 */
	public void setUrl_syntax(String url_syntax) {
		this.url_syntax = url_syntax;
	}
}