package org.gmod.gbol.simpleObject.generated;


import org.gmod.gbol.simpleObject.*; 

import java.util.Date;
import java.util.HashSet;
import java.util.Set;

/**
 * Library generated by hbm2java
 */
public abstract class AbstractLibrary extends AbstractSimpleObject implements java.io.Serializable {


     private Integer libraryId;
     private CVTerm type;
     private Organism organism;
     private String name;
     private String uniqueName;
     private int isObsolete;
     private Date timeAccessioned;
     private Date timeLastModified;
     private Set<LibrarySynonym> librarySynonyms = new HashSet<LibrarySynonym>(0);
     private Set<LibraryDBXref> libraryDBXrefs = new HashSet<LibraryDBXref>(0);
     private Set<LibraryCVTerm> libraryCVTerms = new HashSet<LibraryCVTerm>(0);
     private Set<LibraryProperty> libraryProperties = new HashSet<LibraryProperty>(0);
     private Set<LibraryFeature> libraryFeatures = new HashSet<LibraryFeature>(0);
     private Set<LibraryPublication> libraryPublications = new HashSet<LibraryPublication>(0);

    public AbstractLibrary() {
    }

	
    public AbstractLibrary(CVTerm type, Organism organism, String uniqueName, int isObsolete, Date timeAccessioned, Date timeLastModified) {
        this.type = type;
        this.organism = organism;
        this.uniqueName = uniqueName;
        this.isObsolete = isObsolete;
        this.timeAccessioned = timeAccessioned;
        this.timeLastModified = timeLastModified;
    }
    public AbstractLibrary(CVTerm type, Organism organism, String name, String uniqueName, int isObsolete, Date timeAccessioned, Date timeLastModified, Set<LibrarySynonym> librarySynonyms, Set<LibraryDBXref> libraryDBXrefs, Set<LibraryCVTerm> libraryCVTerms, Set<LibraryProperty> libraryProperties, Set<LibraryFeature> libraryFeatures, Set<LibraryPublication> libraryPublications) {
       this.type = type;
       this.organism = organism;
       this.name = name;
       this.uniqueName = uniqueName;
       this.isObsolete = isObsolete;
       this.timeAccessioned = timeAccessioned;
       this.timeLastModified = timeLastModified;
       this.librarySynonyms = librarySynonyms;
       this.libraryDBXrefs = libraryDBXrefs;
       this.libraryCVTerms = libraryCVTerms;
       this.libraryProperties = libraryProperties;
       this.libraryFeatures = libraryFeatures;
       this.libraryPublications = libraryPublications;
    }
   
    public Integer getLibraryId() {
        return this.libraryId;
    }
    
    public void setLibraryId(Integer libraryId) {
        this.libraryId = libraryId;
    }
    public CVTerm getType() {
        return this.type;
    }
    
    public void setType(CVTerm type) {
        this.type = type;
    }
    public Organism getOrganism() {
        return this.organism;
    }
    
    public void setOrganism(Organism organism) {
        this.organism = organism;
    }
    public String getName() {
        return this.name;
    }
    
    public void setName(String name) {
        this.name = name;
    }
    public String getUniqueName() {
        return this.uniqueName;
    }
    
    public void setUniqueName(String uniqueName) {
        this.uniqueName = uniqueName;
    }
    public int getIsObsolete() {
        return this.isObsolete;
    }
    
    public void setIsObsolete(int isObsolete) {
        this.isObsolete = isObsolete;
    }
    public Date getTimeAccessioned() {
        return this.timeAccessioned;
    }
    
    public void setTimeAccessioned(Date timeAccessioned) {
        this.timeAccessioned = timeAccessioned;
    }
    public Date getTimeLastModified() {
        return this.timeLastModified;
    }
    
    public void setTimeLastModified(Date timeLastModified) {
        this.timeLastModified = timeLastModified;
    }
    public Set<LibrarySynonym> getLibrarySynonyms() {
        return this.librarySynonyms;
    }
    
    public void setLibrarySynonyms(Set<LibrarySynonym> librarySynonyms) {
        this.librarySynonyms = librarySynonyms;
    }
    public Set<LibraryDBXref> getLibraryDBXrefs() {
        return this.libraryDBXrefs;
    }
    
    public void setLibraryDBXrefs(Set<LibraryDBXref> libraryDBXrefs) {
        this.libraryDBXrefs = libraryDBXrefs;
    }
    public Set<LibraryCVTerm> getLibraryCVTerms() {
        return this.libraryCVTerms;
    }
    
    public void setLibraryCVTerms(Set<LibraryCVTerm> libraryCVTerms) {
        this.libraryCVTerms = libraryCVTerms;
    }
    public Set<LibraryProperty> getLibraryProperties() {
        return this.libraryProperties;
    }
    
    public void setLibraryProperties(Set<LibraryProperty> libraryProperties) {
        this.libraryProperties = libraryProperties;
    }
    public Set<LibraryFeature> getLibraryFeatures() {
        return this.libraryFeatures;
    }
    
    public void setLibraryFeatures(Set<LibraryFeature> libraryFeatures) {
        this.libraryFeatures = libraryFeatures;
    }
    public Set<LibraryPublication> getLibraryPublications() {
        return this.libraryPublications;
    }
    
    public void setLibraryPublications(Set<LibraryPublication> libraryPublications) {
        this.libraryPublications = libraryPublications;
    }


   public boolean equals(Object other) {
         if ( (this == other ) ) return true;
		 if ( (other == null ) ) return false;
		 if ( !(other instanceof AbstractLibrary) ) return false;
		 AbstractLibrary castOther = ( AbstractLibrary ) other; 
         
		 return ( (this.getType()==castOther.getType()) || ( this.getType()!=null && castOther.getType()!=null && this.getType().equals(castOther.getType()) ) )
 && ( (this.getOrganism()==castOther.getOrganism()) || ( this.getOrganism()!=null && castOther.getOrganism()!=null && this.getOrganism().equals(castOther.getOrganism()) ) )
 && ( (this.getUniqueName()==castOther.getUniqueName()) || ( this.getUniqueName()!=null && castOther.getUniqueName()!=null && this.getUniqueName().equals(castOther.getUniqueName()) ) );
   }
   
   public int hashCode() {
         int result = 17;
         
         
         result = 37 * result + ( getType() == null ? 0 : this.getType().hashCode() );
         result = 37 * result + ( getOrganism() == null ? 0 : this.getOrganism().hashCode() );
         
         result = 37 * result + ( getUniqueName() == null ? 0 : this.getUniqueName().hashCode() );
         
         
         
         
         
         
         
         
         
         return result;
   }   


}


