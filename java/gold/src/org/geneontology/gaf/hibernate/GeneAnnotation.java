package org.geneontology.gaf.hibernate;

import java.io.Serializable;

import org.geneontology.gold.hibernate.model.Cls;
import org.geneontology.gold.hibernate.model.GOModel;

/**
 * 
 * @author Shahid Manzoor
 *
 */
public class GeneAnnotation extends GOModel implements Serializable {

	private Bioentity bioentity;
	private String qualifierExpression;
	private boolean isContributesTo;
	private boolean isIntegralTo;
	private Cls cls; //TODO: should it be a class or string
	private String referenceId;	
	private String evidenceCls;
	private String withExpression;
	private int actsOnTaxonId;
	private String lastUpdateDate; //TODO: convert it to date
	private String assignedBy;
	private String extensionExpression;
	private String geneProductForm;
	
	public GeneAnnotation(){
		//TODO: TBD
		String keys[] = {"bioentity", "cls", "referenceId", "evidenceCls"};
		this.initUniqueConstraintFields(GeneAnnotation.class, keys);
	}
	
	public GeneAnnotation(Bioentity bioentity, String qualifierExpression,
			boolean isContributesTo, boolean isIntegralTo, Cls cls,
			String referenceId, String evidenceCls, String withExpression,
			int actsOnTaxonId, String lastUpdateDate, String assignedBy,
			String extensionExpression, String geneProductForm) {
		this();
		this.bioentity = bioentity;
		this.qualifierExpression = qualifierExpression;
		this.isContributesTo = isContributesTo;
		this.isIntegralTo = isIntegralTo;
		this.cls = cls;
		this.referenceId = referenceId;
		this.evidenceCls = evidenceCls;
		this.withExpression = withExpression;
		this.actsOnTaxonId = actsOnTaxonId;
		this.lastUpdateDate = lastUpdateDate;
		this.assignedBy = assignedBy;
		this.extensionExpression = extensionExpression;
		this.geneProductForm = geneProductForm;
	}

	public Bioentity getBioentity() {
		return bioentity;
	}

	public void setBioentity(Bioentity bioentity) {
		this.bioentity = bioentity;
	}

	public String getQualifierExpression() {
		return qualifierExpression;
	}

	public void setQualifierExpression(String qualifierExpression) {
		this.qualifierExpression = qualifierExpression;
	}

	public boolean isContributesTo() {
		return isContributesTo;
	}

	public void setContributesTo(boolean isContributesTo) {
		this.isContributesTo = isContributesTo;
	}

	public boolean isIntegralTo() {
		return isIntegralTo;
	}

	public void setIntegralTo(boolean isIntegralTo) {
		this.isIntegralTo = isIntegralTo;
	}

	/**
	 * convenience method for getting the Id of the ontology class
	 * 
	 * @return id of the ontology class
	 */
	public String getClsId() {
		return cls.getId();
	}
	
	public Cls getCls() {
		return cls;
	}

	public void setCls(Cls cls) {
		this.cls = cls;
	}

	public String getReferenceId() {
		return referenceId;
	}

	public void setReferenceId(String referenceId) {
		this.referenceId = referenceId;
	}

	public String getEvidenceCls() {
		return evidenceCls;
	}

	public void setEvidenceCls(String evidenceCls) {
		this.evidenceCls = evidenceCls;
	}

	public String getWithExpression() {
		return withExpression;
	}

	public void setWithExpression(String withExpression) {
		this.withExpression = withExpression;
	}

	public int getActsOnTaxonId() {
		return actsOnTaxonId;
	}

	public void setActsOnTaxonId(int actsOnTaxonId) {
		this.actsOnTaxonId = actsOnTaxonId;
	}

	public String getLastUpdateDate() {
		return lastUpdateDate;
	}

	public void setLastUpdateDate(String lastUpdateDate) {
		this.lastUpdateDate = lastUpdateDate;
	}

	public String getAssignedBy() {
		return assignedBy;
	}

	public void setAssignedBy(String assignedBy) {
		this.assignedBy = assignedBy;
	}

	public String getExtensionExpression() {
		return extensionExpression;
	}

	public void setExtensionExpression(String extensionExpression) {
		this.extensionExpression = extensionExpression;
	}

	public String getGeneProductForm() {
		return geneProductForm;
	}

	public void setGeneProductForm(String geneProductForm) {
		this.geneProductForm = geneProductForm;
	}

	
	
	
}
