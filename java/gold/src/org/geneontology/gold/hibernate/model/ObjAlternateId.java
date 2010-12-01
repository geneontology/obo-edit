package org.geneontology.gold.hibernate.model;

/**
 * 
 * @author Shahid Manzoor
 *
 */
public class ObjAlternateId extends GOModel implements java.io.Serializable {

	private String obj;
	private String id;

	public ObjAlternateId() {

		String keys[] = {"obj", "id"};
		this.initUniqueConstraintFields(ObjAlternateId.class, keys);
	}

	public ObjAlternateId(String obj, String id) {
		this();
		this.obj = obj;
		this.id = id;
	}

	public String getObj() {
		return this.obj;
	}

	public void setObj(String obj) {
		this.obj = obj;
	}

	public String getId() {
		return this.id;
	}

	public void setId(String id) {
		this.id = id;
	}

}
