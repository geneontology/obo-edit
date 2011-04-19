package org.geneontology.gold.hibernate.model;


public class Family extends GOModel {
	 private String id;
	 private String label;
	 private String versioniri;
	 private String creationDate;
	 
	 public Family() {}

	 public Family(String id) {
		 this.id = id;
	 }

	 public Family(String id, String label, String versioniri, String creationDate) {
		 this(id);
		 this.label = label;
		 this.versioniri = versioniri;
		 this.creationDate = creationDate;
	 }

	 
	 public String getId() {
		 return this.id;
	 }
	 
	 public void setId(String id) {
		 this.id = id;
	 }
	 
	 public String getLabel() {
		 return this.label;
	 }
	 
	 public void setLabel(String label) {
		 this.label = label;
	 }
	 
	 public String getVersioniri() {
		 return this.versioniri;
	 }
	 
	 public void setVersioniri(String versioniri) {
		 this.versioniri = versioniri;
	 }
	 
	 public String getCreationDate() {
		 return this.creationDate;
	 }
	 
	 public void setCreationDate(String creationDate) {
		 this.creationDate = creationDate;
	 }
}
