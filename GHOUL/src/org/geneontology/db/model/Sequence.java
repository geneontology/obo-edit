 package org.geneontology.db.model;

import java.sql.Timestamp;
import java.util.Set;

/**
 * The GOSequence class is based on the GO Sequence table.
 * @author Suzanna Lewis
 *
 */

public class Sequence extends GOModel {
	
	/** The primary database key of the Sequence object. */
	protected int seq_id;
	
	/** The primary label used for identifying the sequence for humans. 
	 * Not guaranteed to be globally unique. typically corresponds to the first part of a FASTA header */
	protected String name;
	
	/** textual information for humans concerning this sequence. 
	 * typically corresponds to the part after the ID in the FASTA header  */
	protected String description;
	
	/** residue sequence: standard IUPAC alphabetic codes are used */
	protected String residues;

	/** number of residues in sequence. should always correspond to length(seq), where seq is populated  */
	protected Integer seq_len;

	/** result of md5(seq), where md5 is the standard MD5 checksum algorithm. 
	 * see GO::Model::Seq for calculation 
	 * almost 100% guaranteed to be unique for any sequence of symbols representing the biopolymer */
	protected String md5checksum;

	/** The molecular type (AA or DNA ...) of the residues */
	protected String moltype;

	/** time last modified of the Feature object */
	protected Timestamp timelastmodified;
	
	/** external identifiers for a sequence */
	protected Set<DBXref> dbxrefs;
	
	public Sequence(){
		String[] uniqueConstraintFields = {"name", "md5checksum"};
		this.initUniqueConstraintFields(Sequence.class,uniqueConstraintFields);
	}

	public int getSeq_id() {
		return seq_id;
	}

	public void setSeq_id(int seq_id) {
		this.seq_id = seq_id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getResidues() {
		return residues;
	}

	public void setResidues(String residues) {
		this.residues = residues;
	}

	public Integer getSeq_len() {
		return seq_len;
	}

	public void setSeq_len(Integer seq_len) {
		this.seq_len = seq_len;
	}

	public String getMd5checksum() {
		return md5checksum;
	}

	public void setMd5checksum(String md5checksum) {
		this.md5checksum = md5checksum;
	}

	public String getMoltype() {
		return moltype;
	}

	public void setMoltype(String moltype) {
		this.moltype = moltype;
	}

	public Timestamp getTimelastmodified() {
		return timelastmodified;
	}

	public void setTimelastmodified(Timestamp timelastmodified) {
		this.timelastmodified = timelastmodified;
	}

	public Set<DBXref> getDbxrefs() {
		return dbxrefs;
	}

	public void setDbxrefs(Set<DBXref> dbxrefs) {
		this.dbxrefs = dbxrefs;
	}
}