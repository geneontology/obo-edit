package org.gmod.gbol.simpleObject.generated;


import org.gmod.gbol.simpleObject.*; 

import java.util.HashSet;
import java.util.Set;

/**
 * Study generated by hbm2java
 */
public abstract class AbstractStudy  implements java.io.Serializable {


     private Integer studyId;
     private Publication publication;
     private Contact contact;
     private DBXref dbxref;
     private String name;
     private String description;
     private Set<StudyAssay> studyAssays = new HashSet<StudyAssay>(0);
     private Set<StudyDesign> studyDesigns = new HashSet<StudyDesign>(0);
     private Set<StudyProperty> studyProperties = new HashSet<StudyProperty>(0);

    public AbstractStudy() {
    }

	
    public AbstractStudy(Contact contact, String name) {
        this.contact = contact;
        this.name = name;
    }
    public AbstractStudy(Publication publication, Contact contact, DBXref dbxref, String name, String description, Set<StudyAssay> studyAssays, Set<StudyDesign> studyDesigns, Set<StudyProperty> studyProperties) {
       this.publication = publication;
       this.contact = contact;
       this.dbxref = dbxref;
       this.name = name;
       this.description = description;
       this.studyAssays = studyAssays;
       this.studyDesigns = studyDesigns;
       this.studyProperties = studyProperties;
    }
   
    public Integer getStudyId() {
        return this.studyId;
    }
    
    public void setStudyId(Integer studyId) {
        this.studyId = studyId;
    }
    public Publication getPublication() {
        return this.publication;
    }
    
    public void setPublication(Publication publication) {
        this.publication = publication;
    }
    public Contact getContact() {
        return this.contact;
    }
    
    public void setContact(Contact contact) {
        this.contact = contact;
    }
    public DBXref getDbxref() {
        return this.dbxref;
    }
    
    public void setDbxref(DBXref dbxref) {
        this.dbxref = dbxref;
    }
    public String getName() {
        return this.name;
    }
    
    public void setName(String name) {
        this.name = name;
    }
    public String getDescription() {
        return this.description;
    }
    
    public void setDescription(String description) {
        this.description = description;
    }
    public Set<StudyAssay> getStudyAssays() {
        return this.studyAssays;
    }
    
    public void setStudyAssays(Set<StudyAssay> studyAssays) {
        this.studyAssays = studyAssays;
    }
    public Set<StudyDesign> getStudyDesigns() {
        return this.studyDesigns;
    }
    
    public void setStudyDesigns(Set<StudyDesign> studyDesigns) {
        this.studyDesigns = studyDesigns;
    }
    public Set<StudyProperty> getStudyProperties() {
        return this.studyProperties;
    }
    
    public void setStudyProperties(Set<StudyProperty> studyProperties) {
        this.studyProperties = studyProperties;
    }


   public boolean equals(Object other) {
         if ( (this == other ) ) return true;
		 if ( (other == null ) ) return false;
		 if ( !(other instanceof AbstractStudy) ) return false;
		 AbstractStudy castOther = ( AbstractStudy ) other; 
         
		 return ( (this.getName()==castOther.getName()) || ( this.getName()!=null && castOther.getName()!=null && this.getName().equals(castOther.getName()) ) );
   }
   
   public int hashCode() {
         int result = 17;
         
         
         
         
         
         result = 37 * result + ( getName() == null ? 0 : this.getName().hashCode() );
         
         
         
         
         return result;
   }   


}


