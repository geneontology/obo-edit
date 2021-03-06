package org.geneontology.gold.hibernate.model;

// Generated Nov 4, 2010 4:36:17 PM by Hibernate Tools 3.4.0.Beta1

/**
 * ObjAlternateLabel generated by hbm2java
 */
public class ObjAlternateLabel extends GOModel implements java.io.Serializable {

	private String obj;
	private String label;
	private String synonymScope;
	private String synonymType;
	private String synonymXref;

	public ObjAlternateLabel() {
		String[] uniqueConstaintFields = {"obj", "label", "synonymScope", "synonymType", "synonymXref"};
		this.initUniqueConstraintFields(ObjAlternateLabel.class, uniqueConstaintFields);
	}

	public ObjAlternateLabel(String obj, String label, String synonymScope,
			String synonymType, String synonymXref) {
		this();
		this.obj = obj;
		this.label = label;
		this.synonymScope = synonymScope;
		this.synonymType = synonymType;
		this.synonymXref = synonymXref;
		
	}

	public String getObj() {
		return this.obj;
	}

	public void setObj(String obj) {
		this.obj = obj;
	}

	public String getLabel() {
		return this.label;
	}

	public void setLabel(String label) {
		System.out.println("label.................." + label);
		this.label = label;
	}

	public String getSynonymScope() {
		return this.synonymScope;
	}

	public void setSynonymScope(String synonymScope) {
		this.synonymScope = synonymScope;
	}

	public String getSynonymType() {
		return this.synonymType;
	}

	public void setSynonymType(String synonymType) {
		this.synonymType = synonymType;
	}

	public String getSynonymXref() {
		return this.synonymXref;
	}

	public void setSynonymXref(String synonymXref) {
		this.synonymXref = synonymXref;
	}

	public boolean equals(Object other) {
		System.out.println("***********************");
		
		if ((this == other))
			return true;
		if ((other == null))
			return false;
		if (!(other instanceof ObjAlternateLabel))
			return false;
		ObjAlternateLabel castOther = (ObjAlternateLabel) other;

		return ((this.getObj() == castOther.getObj()) || (this.getObj() != null
				&& castOther.getObj() != null && this.getObj().equals(
				castOther.getObj())))
				&& ((this.getLabel() == castOther.getLabel()) || (this
						.getLabel() != null && castOther.getLabel() != null && this
						.getLabel().equals(castOther.getLabel())))
				&& ((this.getSynonymScope() == castOther.getSynonymScope()) || (this
						.getSynonymScope() != null
						&& castOther.getSynonymScope() != null && this
						.getSynonymScope().equals(castOther.getSynonymScope())))
				&& ((this.getSynonymType() == castOther.getSynonymType()) || (this
						.getSynonymType() != null
						&& castOther.getSynonymType() != null && this
						.getSynonymType().equals(castOther.getSynonymType())))
				&& ((this.getSynonymXref() == castOther.getSynonymXref()) || (this
						.getSynonymXref() != null
						&& castOther.getSynonymXref() != null && this
						.getSynonymXref().equals(castOther.getSynonymXref())));
	}

	public int hashCode() {
		System.out.println("============================== " + getLabel() + ", " + getObj());

		
		int result = 17;

		result = 37 * result
				+ (getObj() == null ? 0 : this.getObj().hashCode());
		result = 37 * result
				+ (getLabel() == null ? 0 : this.getLabel().hashCode());
		result = 37
				* result
				+ (getSynonymScope() == null ? 0 : this.getSynonymScope()
						.hashCode());
		result = 37
				* result
				+ (getSynonymType() == null ? 0 : this.getSynonymType()
						.hashCode());
		result = 37
				* result
				+ (getSynonymXref() == null ? 0 : this.getSynonymXref()
						.hashCode());
		return result;
	}

	
	/*	private ObjAlternateLabelId id;

	public ObjAlternateLabel() {
	}

	public ObjAlternateLabel(ObjAlternateLabelId id) {
		this.id = id;
	}

	public ObjAlternateLabelId getId() {
		return this.id;
	}

	public void setId(ObjAlternateLabelId id) {
		this.id = id;
	}
*/
}
