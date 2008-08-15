package org.geneontology.db.model;

/**
 * The DB class corresponds to the Chado db table.
 * @author Robert Bruggner
 *
 */
public class DB extends GOModel {
	
	/** The db_id of the DB object */
	protected int db_id;

	/** The name of the DB */
	protected String name;

	/** The full name of the DB */
	protected String fullname;

	/** The description of the DB */
	protected String description;

	/** The URL of the DB */
	protected String url;
	
	/** The URL syntax for the DB */
	protected String url_syntax;

	/** An example URL of the DB */
	protected String url_example;

	/** The URL Prefix of the DB */
	protected String uri_prefix;
	
	public DB(){
		String[] uniqueConstraintFields = {"name"};
		this.initUniqueConstraintFields(DB.class,uniqueConstraintFields);
	}

	/**
	 * Getter of DB db_id.
	 * @return the db_id of DB.
	 */
	public int getDb_id() {
		return db_id;
	}

	/**
	 * Setter of DB db_id
	 * @param db_id db_id of DB object.
	 */
	public void setDb_id(int db_id) {
		this.db_id = db_id;
	}

	/**
	 * Getter of DB name.
	 * @return name of DB.
	 */
	public String getName() {
		return name;
	}

	/**
	 * Setter of DB name.
	 * @param name name of DB.
	 */
	public void setName(String name) {
		this.name = name;
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
	 * Getter of DB description.
	 * @return description of DB.
	 */
	public String getDescription() {
		return description;
	}

	/** 
	 * Setter of DB description.
	 * @param description description of DB.
	 */
	public void setDescription(String description) {
		this.description = description;
	}

	/**
	 * Getter of DB url
	 * @return URL of DB.
	 */
	public String getUrl() {
		return url;
	}

	/**
	 * Setter of DB url.
	 * @param url URL of DB.
	 */
	public void setUrl(String url) {
		this.url = url;
	}

	/**
	 * Getter of DB URL Prefix 
	 * @return URL Prefix of DB.
	 */
	public String getUri_prefix() {
		return uri_prefix;
	}

	/**
	 * Setter of DB URL Prefix 
	 * @param urlprefix DB URL Prefix
	 */
	public void setUri_prefix(String uri_prefix) {
		this.uri_prefix = uri_prefix;
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