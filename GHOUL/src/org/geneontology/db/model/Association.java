package org.geneontology.db.model;

import java.util.Set;


/**
 * The GeneProduct class corresponds to the GO gene_product table.  
 * @author Robert Bruggner
 *
 */
public class Association extends GOModel {
		
  	protected int assoc_id;

  	/** the (GO) term to which the gene_product is associated */
  	protected Term term;
  	
  	/** the gene or gene_product to which the term is associated */
  	protected GeneProduct gene_product;
  	
  	/** ** IMPORTANT ** when this field is non-zero, 
  	 * the meaning of the annotation is that the gene_product does NOT have the role defined by the GO term 
  	 * (column 4 = NOT in the gene-association file)
  	 */
  	protected Integer is_not;
  	
  	/**
  	 * a date in YYYYMMDD format. This is the date the association was last checked the source db providers
  	 * (column 14 in the gene-association file) 
  	 */
  	protected Integer date;
 	
  	/** 
  	 * the source of the association; 
  	 * for instance, the association file may come from SwissProt, 
  	 * but the source of the association (Example: SGD) (Example: MGI) 
  	 * (column 15 = NOT in the gene-association file) (docs: http://www.geneontology.org/cgi-bin/xrefs.cgi)
  	 */
  	protected DB source_db;

  	public Association(){
		String[] uniqueConstraintFields = {"term", "gene_product"};
		this.initUniqueConstraintFields(Association.class,uniqueConstraintFields);
	}

	public int getAssoc_id() {
		return assoc_id;
	}

	public void setAssoc_id(int assoc_id) {
		this.assoc_id = assoc_id;
	}

	public Term getTerm() {
		return term;
	}

	public void setTerm(Term term) {
		this.term = term;
	}

	public GeneProduct getGene_product() {
		return gene_product;
	}

	public void setGene_product(GeneProduct gene_product) {
		this.gene_product = gene_product;
	}

	public Integer getIs_not() {
		return is_not;
	}

	public void setIs_not(Integer is_not) {
		this.is_not = is_not;
	}

	public Integer getDate() {
		return date;
	}

	public void setDate(Integer date) {
		this.date = date;
	}

	public DB getSource_db() {
		return source_db;
	}

	public void setSource_db(DB source_db) {
		this.source_db = source_db;
	}

}