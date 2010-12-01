package org.geneontology.gold.hibernate.model;

/**
 * 
 * @author Shahid Manzoor
 *
 */
public class ObjSubset extends GOModel implements java.io.Serializable {

	private String obj;
	private String ontologySubset;

	public ObjSubset() {
		String keys[] = {"obj", "ontologySubset"};
		this.initUniqueConstraintFields(ObjSubset.class, keys);
	}

	public ObjSubset(String obj, String ontologySubset) {
		this();
		this.obj = obj;
		this.ontologySubset = ontologySubset;
	}

	public String getObj() {
		return this.obj;
	}

	public void setObj(String obj) {
		this.obj = obj;
	}

	public String getOntologySubset() {
		return this.ontologySubset;
	}

	public void setOntologySubset(String ontologySubset) {
		this.ontologySubset = ontologySubset;
	}

}
