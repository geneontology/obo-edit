 package org.geneontology.db.model;

import java.sql.Timestamp;

/**
 * The GOSequence class is based on the GO Sequence table.
 * @author Suzanna Lewis
 *
 */

public class Sequence extends GOModel {
	
	/** The feature_id of the Feature object. */
	protected int feature_id;
	/** The name of the Feature object */
	protected String name;
	/** The {@link CVTerm} type of the Feature object */
	protected Term type;
	/** The {@link DBXref} of the Feature object */
	protected DBXref dbxref;
		/** The residues of the Feature object */
	protected String residues;
	/** The length of the Feature object */
	protected Integer seqlen;
	/** The md5 checksum of the residues, maybe? */
	protected String md5checksum;
	/** The molecular type (AA or DNA ...) of the residues */
	protected String moltype;
	/** time last modified of the Feature object */
	protected Timestamp timelastmodified;
	
	public Sequence(){
		String[] uniqueConstraintFields = {"organism","uniquename","type"};
		this.initUniqueConstraintFields(Sequence.class,uniqueConstraintFields);
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
	public int getFeature_id() {
		return feature_id;
	}
	public void setFeature_id(int feature_id) {
		this.feature_id = feature_id;
	}
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public Term getType() {
		return type;
	}
	public void setType(Term type) {
		this.type = type;
	}
	public DBXref getDbxref() {
		return dbxref;
	}
	public void setDbxref(DBXref dbxref) {
		this.dbxref = dbxref;
	}
	public String getResidues() {
		return residues;
	}
	public void setResidues(String residues) {
		this.residues = residues;
	}
	public Integer getSeqlen() {
		return seqlen;
	}
	public void setSeqlen(Integer seqlen) {
		this.seqlen = seqlen;
	}
	public String getMd5checksum() {
		return md5checksum;
	}
	public void setMd5checksum(String md5checksum) {
		this.md5checksum = md5checksum;
	}
	public Timestamp getTimelastmodified() {
		return timelastmodified;
	}
	public void setTimelastmodified(Timestamp timelastmodified) {
		this.timelastmodified = timelastmodified;
	}	

}