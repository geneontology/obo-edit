package org.geneontology.db.model;

import java.io.Serializable;

public class ProductSeq  extends GOModel implements Serializable {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/** Represents a gene or gene_product, typically at the species level. */
	private GeneProduct gene_product;
	
	/** A representative DNA or amino acid sequence for a gene_product; will typically be amino acid. 
	 * This table is modeled after the BioPerl Bio::PrimarySeq model
	 * 
	 */
	private Sequence seq;
	
	/** If this link is for the representative sequence for a gene product, is_primary_seq should be set to true (=1) */
	private Integer is_primary_seq;
		
	public ProductSeq(){
		final String[] fieldNames = {"gene_product", "seq"};
		this.initUniqueConstraintFields(ProductSeq.class, fieldNames);
	}

	public GeneProduct getGene_product() {
		return gene_product;
	}


	public void setGene_product(GeneProduct gene_product) {
		this.gene_product = gene_product;
	}


	public Sequence getSeq() {
		return seq;
	}

	public void setSeq(Sequence seq) {
		this.seq = seq;
	}


	public Integer getIs_primary_seq() {
		return is_primary_seq;
	}


	public void setIs_primary_seq(Integer is_primary_seq) {
		this.is_primary_seq = is_primary_seq;
	}

	public static long getSerialVersionUID() {
		return serialVersionUID;
	}
	
}