package org.geneontology.db.model;

/**
 * The DB class corresponds to the Chado db table.
 * @author Robert Bruggner
 *
 */
public class DBXref extends GOModel {
	
	/** the dbxref_id of the DBXref object */
	protected int dbxref_id;
	
	/** for backup also keep the DB name here because not all of them can be found in DB */
	protected String db_name;
	
	/** the accession of this DBXRef object */
	protected String accession;
	
	protected String keytype;
			
	/** a description of this DBXref object */
	protected String description;
	
	/** whether dbxref is for a term definition */
	boolean for_definition;

	public DBXref(){
		String[] uniqueConstraintFields = {"db_name", "accession"};
		this.initUniqueConstraintFields(DBXref.class, uniqueConstraintFields);
	}
	
	/**
	 * Getter of DBXref dbxref_id
	 * @return dbxref_id of DBXref object.
	 */
	public int getDbxref_id() {
		return dbxref_id;
	}
	
	/**
	 * Setter of DBXref dbxref_id.
	 * @param dbxref_id dbxref_id of this DBXref object.
	 */
	public void setDbxref_id(int dbxref_id) {
		this.dbxref_id = dbxref_id;
	}
	
	public String getDb_name() {
		return db_name;
	}

	public void setDb_name(String db_name) {
		this.db_name = db_name;
	}

	/**
	 * Getter of DBXref accession.
	 * @return accession number of DBXref object.
	 */
	public String getAccession() {
		return accession;
	}
	
	/**
	 * Setter of DBXref accession.
	 * @param accession accession number of DBXref object.
	 */
	public void setAccession(String accession) {
		this.accession = accession;
	}
	
	/**
	 * Getter of DBXref description.
	 * @return description of DBXref object.
	 */
	public String getDescription() {
		return description;
	}
	
	/**
	 * Setter of DBXref description
	 * @param description description of DBXref object.
	 */
	public void setDescription(String description) {
		this.description = description;
	}
	
	public String getKeytype() {
		return keytype;
	}

	public void setKeytype(String keytype) {
		this.keytype = keytype;
	}

	public boolean isFor_definition() {
		return for_definition;
	}

	public void setFor_definition(boolean for_definition) {
		this.for_definition = for_definition;
	}

}