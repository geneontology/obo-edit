package org.geneontology.gold.hibernate.model;

// Generated Nov 2, 2010 3:53:51 PM by Hibernate Tools 3.4.0.Beta1

/**
 * ObjXref generated by hbm2java
 */
public class ObjXref extends GOModel implements java.io.Serializable {

	private String obj;
	private String xref;
	private String xrefDescription;

	public ObjXref() {
		String keys[] = {"obj", "xref"};
		this.initUniqueConstraintFields(ObjXref.class, keys);
	}

	public ObjXref(String obj, String xref, String xrefDescription) {
		this();
		this.obj = obj;
		this.xref = xref;
		this.xrefDescription = xrefDescription;
	}

	public String getObj() {
		return this.obj;
	}

	public void setObj(String obj) {
		this.obj = obj;
	}

	public String getXref() {
		return this.xref;
	}

	public void setXref(String xref) {
		this.xref = xref;
	}

	public String getXrefDescription() {
		return this.xrefDescription;
	}

	public void setXrefDescription(String xrefDescription) {
		this.xrefDescription = xrefDescription;
	}

}
