package org.geneontology.db.model;

/**
 * The Species class corresponds to the GO species table.  
 * @author Robert Bruggner
 *
 */
public class Species extends GOModel {
		
	protected int species_id;
	
	protected int ncbi_taxa_id;
	
	protected String common_name;

	protected String lineage_string;
	
	protected String genus;

	protected String species;

	protected Species parent;
	
	protected int left_value;
	
	protected int right_value;
	
	protected String taxonomic_rank;
	
	public Species(){
		String[] uniqueConstraintFields = {"ncbi_taxa_id"};
		this.initUniqueConstraintFields(Species.class,uniqueConstraintFields);
	}

	public int getSpecies_id() {
		return species_id;
	}

	public void setSpecies_id(int species_id) {
		this.species_id = species_id;
	}

	public int getNcbi_taxa_id() {
		return ncbi_taxa_id;
	}

	public void setNcbi_taxa_id(int ncbi_taxa_id) {
		this.ncbi_taxa_id = ncbi_taxa_id;
	}

	public String getCommon_name() {
		return common_name;
	}

	public void setCommon_name(String common_name) {
		this.common_name = common_name;
	}

	public String getLineage_string() {
		return lineage_string;
	}

	public void setLineage_string(String lineage_string) {
		this.lineage_string = lineage_string;
	}

	public String getGenus() {
		return genus;
	}

	public void setGenus(String genus) {
		this.genus = genus;
	}

	public String getSpecies() {
		return species;
	}

	public void setSpecies(String species) {
		this.species = species;
	}

	public Species getParent() {
		return parent;
	}

	public void setParent(Species parent) {
		this.parent = parent;
	}

	public int getLeft_value() {
		return left_value;
	}

	public void setLeft_value(int left_value) {
		this.left_value = left_value;
	}

	public int getRight_value() {
		return right_value;
	}

	public void setRight_value(int right_value) {
		this.right_value = right_value;
	}

	public String getTaxonomic_rank() {
		return taxonomic_rank;
	}

	public void setTaxonomic_rank(String taxonomic_rank) {
		this.taxonomic_rank = taxonomic_rank;
	}

}