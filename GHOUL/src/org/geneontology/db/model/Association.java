package org.geneontology.db.model;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;
import java.util.TimeZone;

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

	/**
	 *  each association can have one or more pieces of evidence attached to it 
	 *  (the schema actually allows zero or more, but with GO all annotation have at least one piece of evidence) 
	 *  (doc: http://www.geneontology.org/GO.evidence.shtml)
	 */
	protected Set<Evidence> evidence;

	protected Set<Term> qualifiers;

	/**
	 * This field is not yet in the database, but I think it needs to be
	 * It indicates that this particular association of gene to term has
	 * inherited from an ancestral protein
	 */
	protected boolean is_inherited;
	
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

	public boolean isNot() {
		return (is_not != null && is_not == 1);
	}

	public void setNot(boolean not) {
		this.is_not = not ? 1 : 0;
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

	public Set<Evidence> getEvidence() {
		return evidence;
	}

	public void setEvidence(Set<Evidence> evidence) {
		this.evidence = evidence;
	}

	public void addEvidence(Evidence evidence) {
		if (this.evidence == null)
			this.evidence = new HashSet<Evidence> ();
		this.evidence.add(evidence);
		evidence.setAssociation(this);
	}

	public String toString() {
		return getGene_product().toString()+" "+getTerm().toString();
	}

	public Set<Term> getQualifiers() {
		return qualifiers;
	}

	public void setQualifiers(Set<Term> qualifiers) {
		this.qualifiers = qualifiers;
	}

	public void setDate() {
		long timestamp = System.currentTimeMillis();
		/* Date appears to be fixed?? */
  		Date when = new Date(timestamp);
  		SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMdd");
  		sdf.setTimeZone(TimeZone.getDefault()); // local time
  		String date_str = sdf.format(when);
  		setDate(Integer.valueOf(date_str));
	}

	public boolean isInherited() {
		return is_inherited;
	}

	public void setInherited(boolean is_inherited) {
		this.is_inherited = is_inherited;
	}
}