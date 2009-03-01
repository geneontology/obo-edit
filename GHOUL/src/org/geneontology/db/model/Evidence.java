package org.geneontology.db.model;

import java.util.HashSet;
import java.util.Set;


/**
 * The Evidence class corresponds to the GO evidence table.  
 * @author Suzi Lewis
 *
 */
public class Evidence extends GOModel {
		
	public static final String [] [] evidence_codes = {
		{"EXP", "Experimental data"},
		{"IC", "Curator"},
		{"IDA", "Direct Assay"},
		{"IEA", "Electronic Analysis"},
		{"IEP", "Expression Pattern"},
		{"IGC", "Genomic Context"},
		{"IGI", "Genetic Interaction"},
		{"IMP", "Mutant Phenotype"},
		{"IPI", "Physical Interaction"},
		{"ISA", "Sequence Alignment"},
		{"ISM", "Sequence Model Similarity"},
		{"ISO", "Sequence Orthology"},
		{"ISS", "Sequence or Structural Similarity"},
		{"NAS", "Not-traceable Author Statement"},
		{"ND", "No data available"},
		{"NR", "Not Recorded"},
		{"RCA", "Reviewed Computational Analysis"},
		{"TAS", "Traceable Author Statement"},
	};
		
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
		if (withs == null) {
			withs = new HashSet<DBXref> ();
		}
		return withs;
	}


	public void setWiths(Set<DBXref> withs) {
		this.withs = withs;
	}

	public void addWith(DBXref with) {
		if (withs == null) {
			withs = new HashSet<DBXref> ();
		}
		this.withs.add(with);
	}

	public String getTip() {
		String tip = null;
		for (int i = 0; i < evidence_codes.length && tip == null; i++) {
			if (evidence_codes[i][0].equals(code))
				tip = evidence_codes[i][1];
		}
		return tip;
	}
}