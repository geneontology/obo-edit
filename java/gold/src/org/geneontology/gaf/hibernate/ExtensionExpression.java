package org.geneontology.gaf.hibernate;

import java.io.Serializable;

import org.geneontology.gold.hibernate.model.Cls;
import org.geneontology.gold.hibernate.model.GOModel;
import org.geneontology.gold.hibernate.model.Relation;

/**
 * The class represents the extension_expression table in the database. Please see ExtensionExpression.hbm.xml file in this 
 * for the mapping detail 
 * @author Shahid Manzoor
 *
 */

public class ExtensionExpression extends owltools.gaf.ExtensionExpression implements Serializable {

	
	private Relation relationObj;
	private boolean isRelationObjLoaded;
	
	private Cls clsObj;
	private boolean isClsObjLoaded;
	
	public ExtensionExpression() {
		super();
	}
	public ExtensionExpression(String id, String relation, String cls) {
		super(id, relation, cls);
	}

	public Relation getRelationObj() {
		if(relationObj == null && relation != null && !isRelationObjLoaded){
			isRelationObjLoaded = true;
			relationObj = (Relation)GOModel.getHibernateObject(Relation.class, "id", getRelation());
		}
		
		return relationObj;
	}
	public void setRelationObj(Relation relationObj) {
		this.relationObj = relationObj;
	}
	public Cls getClsObj() {
		if(clsObj == null && cls != null && !isClsObjLoaded){
			isClsObjLoaded = true;
			clsObj = (Cls) GOModel.getHibernateObject(Cls.class, "id", getCls());
		}
		
		return clsObj;
	}
	public void setClsObj(Cls clsObj) {
		this.clsObj = clsObj;
	}
	
	/**
	 * This method generates unique hashcode used by hibernate.
	 */
	public int hashCode() {
		int result = 17;
	
		result = 37 * result + (this.getId() == null ? 0 : getId().hashCode());
		result = 37 * result + (this.getCls() == null ? 0 : getCls().hashCode());
		result = 37 * result + (this.getRelation() == null ? 0 : getRelation().hashCode());
		
		return result;
	}	
	
	public boolean equals(Object other) {
		if(this == other)
			return true;
		
		if(other == null)
			return false;
		
		if(!(other instanceof ExtensionExpression))
			return false;
		
		ExtensionExpression ee = (ExtensionExpression) other;
		
		boolean result = ( getId() == ee.getId() || getId() != null && getId().equals(ee.getId()) )
				&& (getCls() == ee.getCls() || getCls() != null && getCls().equals(ee.getCls()))
				&& (getRelation() == ee.getRelation() || getRelation() != null && getRelation().equals(ee.getRelation()));
		
		return result;
	}	
	
}
