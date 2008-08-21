package org.geneontology.db.model;

import java.util.Set;


/**
 * The GeneProduct class corresponds to the GO gene_product table.  
 * @author Robert Bruggner
 *
 */
public class HomolSet extends GOModel {
		
  	protected int homolset_id;

  	/** 
  	 * A convenient human-assigned label for the homology set; 
  	 * this may be arbitarily chosen from one of the set members (frequently the target_gene_product); 
  	 * or it may be the name of the gene family. no guarantee is given to its usefulness
  	 */
  	protected String symbol; 	

  	/** 
  	 * A globally unique identifier for this set, or some proxy for the set. 
  	 * For example, if the purpose of building homolsets is to examine disease genes, 
  	 * then an OMIM ID may be appropriate to use here, even though OMIM is not concerned with homology or orthology. 
  	 * If the homolset is derived go_associations.
	*/
  	protected DBXref dbxref;
  	
  	/**
  	 * A homolset may be constructed from a collection of pairwise homology assignments between individual gene products 
  	 * and a (possibly arbitrary) "target" gene_product. this field is optional: 
  	 * for example, a target is not required for sets that are derived from a tree-based analysis
  	 */
  	protected GeneProduct target_gp; 	

  	/**
  	 * The least common ancestor of all members of the homolset. (may not be populated)
  	 */
	protected Species species;

	/**
	 * homolsets may fall into different categories - this field identifies the category. 
	 * May not be populated
	 */
	protected Term term;
	
	/**
	 * All of the genes that are in this homolset
	 */
	protected Set<GeneProduct> genes;
	
	/**
	 * for example: if the purpose of the homolset is to examine disease genes and model organism orthologs, 
	 * the description could be a summary of the disease in human 
	 */
	protected String description;	
	
	public HomolSet(){
		String[] uniqueConstraintFields = {"dbxref"};
		this.initUniqueConstraintFields(HomolSet.class,uniqueConstraintFields);
	}

	public int getHomolset_id() {
		return homolset_id;
	}

	public void setHomolset_id(int homolset_id) {
		this.homolset_id = homolset_id;
	}

	public String getSymbol() {
		return symbol;
	}

	public void setSymbol(String symbol) {
		this.symbol = symbol;
	}

	public DBXref getDbxref() {
		return dbxref;
	}

	public void setDbxref(DBXref dbxref) {
		this.dbxref = dbxref;
	}

	public GeneProduct getTarget_gp() {
		return target_gp;
	}

	public void setTarget_gp(GeneProduct target_gp) {
		this.target_gp = target_gp;
	}

	public Species getSpecies() {
		return species;
	}

	public void setSpecies(Species species) {
		this.species = species;
	}

	public Term getTerm() {
		return term;
	}

	public void setTerm(Term term) {
		this.term = term;
	}

	public Set<GeneProduct> getGenes() {
		return genes;
	}

	public void setGenes(Set<GeneProduct> genes) {
		this.genes = genes;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

}