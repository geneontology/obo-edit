package org.geneontology.db.model;

import org.gmod.db.chAPI.simpleObject.DBXref;

/**
 * The DB class corresponds to the Chado db table.
 * @author Robert Bruggner
 *
 */
public class GOXref extends DBXref {
	
	protected String keytype;
			
	public String getKeytype() {
		return keytype;
	}

	public void setKey_type(String keytype) {
		this.keytype = keytype;
	}

	public GOXref(){
		String[] uniqueConstraintFields = {"db","accession"};
		this.initUniqueConstraintFields(DBXref.class, uniqueConstraintFields);
	}
	
}