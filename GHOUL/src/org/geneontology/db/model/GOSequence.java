 package org.geneontology.db.model;

import org.gmod.db.chAPI.simpleObject.Feature;

/**
 * The GOSequence class is based on the GO Sequence table.
 * @author Suzanna Lewis
 *
 */
public class GOSequence extends Feature {
	
	/** The molecular type (AA or DNA ...) of the residues */
	protected String moltype;
	
	public GOSequence(){
		String[] uniqueConstraintFields = {"organism","uniquename","type"};
		this.initUniqueConstraintFields(GOSequence.class,uniqueConstraintFields);
	}
	/**
	 * Getter of Feature moltype
	 * @return moltype of Feature
	 */
	public String getMoltype() {
		return moltype;
	}
	
	/**
	 * Setter of Feature MD5 Checksum
	 * @param md5checksum MD5 Checksum of Feature.
	 */
	public void setMoltype(String moltype) {
		this.moltype = moltype;
	}	

}