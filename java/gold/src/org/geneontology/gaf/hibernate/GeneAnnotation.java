package org.geneontology.gaf.hibernate;

import java.io.Serializable;
import org.geneontology.gold.hibernate.model.Cls;
import org.geneontology.gold.hibernate.model.GOModel;

import owltools.gaf.Bioentity;

/**
 * The class represents the gene_annotation table in the database. Please see GeneAnnotation.hbm.xml file in this 
 * for the mapping detail 
 * @author Shahid Manzoor
 *
 */
public class GeneAnnotation extends owltools.gaf.GeneAnnotation implements Serializable {

	private transient Cls clsObject;
	private boolean isClsObjectLoaded;
	
	private Cls actsOnTaxonIdObject;
	private boolean isActsOnTaxonIdObjectLoaded;
	
	private transient boolean isBioentityObjectLoaded;
	
	public GeneAnnotation() {
		super();
	}
	
	public Cls getClsObject() {
		return clsObject;
	}

	public Cls getActsOnTaxonIdObject() {
		if(actsOnTaxonIdObject == null && !isActsOnTaxonIdObjectLoaded && actsOnTaxonId != null){
			isActsOnTaxonIdObjectLoaded = true;
			this.isChanged = true;
			actsOnTaxonIdObject = (Cls)GOModel.getHibernateObject(Cls.class, "id", getActsOnTaxonId());
		}
		
		return actsOnTaxonIdObject;
	}

	public void setClsObject(Cls clsObject) {
		if(this.cls != null && this.clsObject == null && !isClsObjectLoaded){
			isClsObjectLoaded = true;
			this.isChanged = true;
			clsObject = (Cls) GOModel.getHibernateObject(Cls.class, "id", getCls());
		}
		
		this.clsObject = clsObject;
	}


	public void setActsOnTaxonIdObject(Cls actsOnTaxonIdObject) {
		this.actsOnTaxonIdObject = actsOnTaxonIdObject;
	}


	public Bioentity getBioentityObject() {
		if(bioentityObject == null && this.bioentity != null && !this.isBioentityObjectLoaded){
			this.isBioentityObjectLoaded = true;
			this.isChanged = true;
			bioentityObject = (org.geneontology.gaf.hibernate.Bioentity) GOModel.getHibernateObject(org.geneontology.gaf.hibernate.Bioentity.class, "id", getBioentity());
		}
		return bioentityObject;
	}

	
	public GeneAnnotation(String bioentity, boolean isContributesTo,
			boolean isIntegralTo, String compositeQualifier, String cls,
			String referenceId, String evidenceCls, String withExpression,
			String actsOnTaxonId, String lastUpdateDate, String assignedBy,
			String extensionExpression, String geneProductForm,
			String gafDocument) {
		super(bioentity, isContributesTo, isIntegralTo, compositeQualifier, cls,
				referenceId, evidenceCls, withExpression, actsOnTaxonId,
				lastUpdateDate, assignedBy, extensionExpression, geneProductForm,
				gafDocument);
	}

	/**
	 * This method generates unique hashcode used by hibernate.
	 */
	public int hashCode() {
		int result = 37;
		
		result = 37 * result + (getBioentity() == null ? 0 : getBioentity().hashCode());
		result = 37 * result + (getCls() == null ? 0 : getCls().hashCode());
		result = 37 * result + (getReferenceId() == null ? 0 : getReferenceId().hashCode());
		result = 37 * result + (getEvidenceCls() == null ? 0 : getEvidenceCls().hashCode());
		result = 37 * result + (getCompositeQualifier() == null ? 0 : getCompositeQualifier().hashCode());
		result = 37 * result + (getWithExpression() == null ? 0 : getWithExpression().hashCode());
		result = 37 * result + (getLastUpdateDate() == null ? 0 : getLastUpdateDate().hashCode());
		result = 37 * result + (getActsOnTaxonId() == null ? 0 : getActsOnTaxonId().hashCode());
		result = 37 * result + (getAssignedBy() == null ? 0 : getAssignedBy().hashCode());
		
		
		return result;
	}
	
	public boolean equals(Object other) {
		if(this == other)
			return true;
		
		if(other == null)
			return false;
		
		if((other instanceof GeneAnnotation))
			return false;
		
		GeneAnnotation ga = (GeneAnnotation) other;
		
		boolean ret = (getBioentity() == ga.getBioentity() || (getBioentity() != null && ga.getBioentity() != null 
				&& getBioentity().equals(ga.getBioentity()) ) )
				&& (getCls() == ga.getCls() || (getCls() != null && getCls().equals(ga.getCls())))
				&& (getReferenceId() == ga.getReferenceId() || (getReferenceId() != null && getReferenceId().equals(ga.getReferenceId())))
				&& (getEvidenceCls() == ga.getEvidenceCls() || (getEvidenceCls() != null && getEvidenceCls().equals(ga.getEvidenceCls())))
				&& (getCompositeQualifier() == ga.getCompositeQualifier() || (getCompositeQualifier() != null && getCompositeQualifier().equals(ga.getCompositeQualifier())))
				&& (getWithExpression() == ga.getWithExpression() || (getWithExpression() != null && getWithExpression().equals(ga.getWithExpression())))
				&& (getLastUpdateDate() == ga.getLastUpdateDate() || (getLastUpdateDate() != null && getLastUpdateDate().equals(ga.getLastUpdateDate())))
				&& (getActsOnTaxonId() == ga.getActsOnTaxonId() || (getActsOnTaxonId() != null && getActsOnTaxonId().equals(ga.getActsOnTaxonId())))
				&& (getAssignedBy() == ga.getAssignedBy() || (getAssignedBy() != null && getAssignedBy().equals(ga.getAssignedBy())))
		;
		
		
		return ret;
		
			
	}

	

}
