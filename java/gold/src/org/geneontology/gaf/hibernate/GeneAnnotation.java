package org.geneontology.gaf.hibernate;

import java.io.Serializable;
import java.util.List;

import org.geneontology.gold.hibernate.model.Cls;
import org.geneontology.gold.hibernate.model.GOModel;

/**
 * 
 * @author Shahid Manzoor
 *
 */
public class GeneAnnotation extends GOModel implements Serializable {

	private String bioentity;
	private transient Bioentity bioentityObject;
//	private String qualifierExpression;
	private boolean isContributesTo;
	private boolean isIntegralTo;
	private String compositeQualifier;
	private String cls;
	private transient Cls clsObject;
	private String referenceId;	
	private String evidenceCls;
	private String withExpression;
	private int actsOnTaxonId;
	private String lastUpdateDate; //TODO: convert it to date
	private String assignedBy;
	private String extensionExpression;
	private String geneProductForm;
	private String gafDocument;
	
	public GeneAnnotation(){
		//TODO: TBD
		String keys[] = {"bioentity", "cls", "referenceId", "evidenceCls"};
		this.initUniqueConstraintFields(GeneAnnotation.class, keys);
	}
	
	
	
	public GeneAnnotation(String bioentity, boolean isContributesTo,
			boolean isIntegralTo, String compositeQualifier, String cls,
			String referenceId, String evidenceCls, String withExpression,
			int actsOnTaxonId, String lastUpdateDate, String assignedBy,
			String extensionExpression, String geneProductForm,
			String gafDocument) {
		this();
		this.bioentity = bioentity;
		this.isContributesTo = isContributesTo;
		this.isIntegralTo = isIntegralTo;
		this.compositeQualifier = compositeQualifier;
		this.cls = cls;
		this.referenceId = referenceId;
		this.evidenceCls = evidenceCls;
		this.withExpression = withExpression;
		this.actsOnTaxonId = actsOnTaxonId;
		this.lastUpdateDate = lastUpdateDate;
		this.assignedBy = assignedBy;
		this.extensionExpression = extensionExpression;
		this.geneProductForm = geneProductForm;
		this.gafDocument = gafDocument;
	}



	public String getBioentity() {
		return bioentity;
	}

	public void setBioentity(String bioentity) {
		this.bioentity = bioentity;
	}

	public String getCls() {
		return cls;
	}

	public void setCls(String cls) {
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
	

	/*
	public String getQualifierExpression() {
		return qualifierExpression;
	}

	public void setQualifierExpression(String qualifierExpression) {
		this.qualifierExpression = qualifierExpression;
	}*/



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


	public String getCompositeQualifier() {
		return compositeQualifier;
	}

	public void setCompositeQualifier(String compositeQualifier) {
		this.compositeQualifier = compositeQualifier;
	}

	public Bioentity getBioentityObject() {
		if(bioentityObject == null)
			bioentityObject =(Bioentity) getHibernateObject(Bioentity.class, "id", getBioentity());
		
		return bioentityObject;
	}

	
	public void setBioentityObject(Bioentity bioentityObject) {
		this.bioentityObject = bioentityObject;
	}
	
	
	public Cls getClsObject() {
		if(clsObject == null)
			clsObject = (Cls)getHibernateObject(Cls.class, "id", getCls());
		
		
		return clsObject;
	}

	
	public void setClsObject(Cls clsObject) {
		this.clsObject = clsObject;
	}

	public String getGafDocument() {
		return gafDocument;
	}

	public void setGafDocument(String gafDocument) {
		this.gafDocument = gafDocument;
	}

	public boolean getIsContributesTo() {
		return isContributesTo;
	}

	public void setIsContributesTo(boolean isContributesTo) {
		this.isContributesTo = isContributesTo;
	}

	public boolean getIsIntegralTo() {
		return isIntegralTo;
	}

	public void setIsIntegralTo(boolean isIntegralTo) {
		this.isIntegralTo = isIntegralTo;
	}

	public List<ExtensionExpression> getExtensionExpressions(){
		return null;
	}
	
	public List<WithInfo> getWithInfos(){
		return null;
	}
	
	public List<CompositeQualifier> getCompositeQualifiers(){
		return null;
	}
	
}
