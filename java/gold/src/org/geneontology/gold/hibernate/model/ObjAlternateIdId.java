package org.geneontology.gold.hibernate.model;

// Generated Oct 22, 2010 9:25:51 AM by Hibernate Tools 3.4.0.Beta1

/**
 * ObjAlternateIdId generated by hbm2java
 */
public class ObjAlternateIdId implements java.io.Serializable {

	private String obj;
	private String id;

	public ObjAlternateIdId() {
	}

	public ObjAlternateIdId(String obj, String id) {
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

	public boolean equals(Object other) {
		if ((this == other))
			return true;
		if ((other == null))
			return false;
		if (!(other instanceof ObjAlternateIdId))
			return false;
		ObjAlternateIdId castOther = (ObjAlternateIdId) other;

		return ((this.getObj() == castOther.getObj()) || (this.getObj() != null
				&& castOther.getObj() != null && this.getObj().equals(
				castOther.getObj())))
				&& ((this.getId() == castOther.getId()) || (this.getId() != null
						&& castOther.getId() != null && this.getId().equals(
						castOther.getId())));
	}

	public int hashCode() {
		int result = 17;

		result = 37 * result
				+ (getObj() == null ? 0 : this.getObj().hashCode());
		result = 37 * result + (getId() == null ? 0 : this.getId().hashCode());
		return result;
	}

}
