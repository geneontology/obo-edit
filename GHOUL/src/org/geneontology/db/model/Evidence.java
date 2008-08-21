package org.geneontology.db.model;

import java.util.Set;


/**
 * The GeneProduct class corresponds to the GO gene_product table.  
 * @author Robert Bruggner
 *
 */
public class Evidence extends GOModel {
		
  	protected int evidence_id;

  	/** 
  	 * a string code (typically 3-letter) corresponding to a GO evidence code. 
  	 * (column 7 in the gene-association file) 
  	 * (Example: IEA - inferred from electronic annotation) 
  	 * (Example: IMP - inferred from mutant phenotype) 
  	 * evidence codes have become "ontologized", but we have not yet taken advantage of the OBO evidence ontology: 
  	 * 		http://www.obofoundry.org/cgi-bin/detail.cgi?evidence_code 
  	 */
  	protected String code;
  	
  	/** the (GO) term to which the gene_product is associated */
  	protected Association association;
  	
  	/** A reference for the annotation. Typically a pubmed ID (column 6 in the gene-association file)  */
  	protected DBXref dbxref;
  	  	
  	/**
  	 *  each piece of evidence can have multiple dbxrefs associated with it; 
  	 *  this is the *normalised* version of the "With" or "From" field of the evidence 
  	 *  (column 8 in the gene-association file, normalized)
  	 */
  	protected Set<DBXref> withs;
   	
  	public Evidence(){
		String[] uniqueConstraintFields = {"code", "association", "dbxref"};
		this.initUniqueConstraintFields(Evidence.class,uniqueConstraintFields);
	}


	public int getEvidence_id() {
		return evidence_id;
	}


	public void setEvidence_id(int evidence_id) {
		this.evidence_id = evidence_id;
	}


	public String getCode() {
		return code;
	}


	public void setCode(String code) {
		this.code = code;
	}


	public Association getAssociation() {
		return association;
	}


	public void setAssociation(Association association) {
		this.association = association;
	}


	public DBXref getDbxref() {
		return dbxref;
	}


	public void setDbxref(DBXref dbxref) {
		this.dbxref = dbxref;
	}


	public Set<DBXref> getWiths() {
		return withs;
	}


	public void setWiths(Set<DBXref> withs) {
		this.withs = withs;
	}

}