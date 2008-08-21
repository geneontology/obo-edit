package org.geneontology.db.model;

import java.util.Set;


/**
 * The GeneProduct class corresponds to the GO gene_product table.  
 * @author Robert Bruggner
 *
 */
public class GeneProduct extends GOModel {
		
	/** The gene_product id */
	protected int gp_id;
	
	/** concise label for this gene product */
	protected String symbol;

	/** A globally unique identifier for this gene or gene product */
	protected DBXref dbxref;

	/** The species or taxon to which this gene product belongs */
	protected Species species;

	/** gene_product type (eg gene, transcript, protein, complex) (column 13 in the gene-association file) */
	protected Term SO_type;
	
	/** symbol is typically a concise label, full_name may be more textual (column 10 in the gene-association file) */
	protected String full_name;
	
	/** alternate labels for the gene or gene product (column 11 in the gene-association file) */
	protected Set<String> synonyms;
		
	protected Set<ProductSeq> seqs;
	
	protected HomolSet homol_set;
	
	public GeneProduct(){
		String[] uniqueConstraintFields = {"dbxref"};
		this.initUniqueConstraintFields(GeneProduct.class,uniqueConstraintFields);
	}

	public int getGp_id() {
		return gp_id;
	}

	public void setGp_id(int gp_id) {
		this.gp_id = gp_id;
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

	public Species getSpecies() {
		return species;
	}

	public void setSpecies(Species species) {
		this.species = species;
	}

	public Term getSO_type() {
		return SO_type;
	}

	public void setSO_type(Term so_type) {
		SO_type = so_type;
	}

	public String getFull_name() {
		return full_name;
	}

	public void setFull_name(String full_name) {
		this.full_name = full_name;
	}

	public Set<String> getSynonyms() {
		return synonyms;
	}

	public void setSynonyms(Set<String> synonyms) {
		this.synonyms = synonyms;
	}

	public Set<ProductSeq> getSeqs() {
		return seqs;
	}

	public void setSeqs(Set<ProductSeq> seqs) {
		this.seqs = seqs;
	}

	public HomolSet getHomol_set() {
		return homol_set;
	}

	public void setHomol_set(HomolSet homol_set) {
		this.homol_set = homol_set;
	}
	
}