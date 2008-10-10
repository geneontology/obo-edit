package org.gmod.gbol.simpleObject.generated;


import org.gmod.gbol.simpleObject.*; 


/**
 * LibraryFeature generated by hbm2java
 */
public abstract class AbstractLibraryFeature extends AbstractSimpleObject implements java.io.Serializable {


     private Integer libraryFeatureId;
     private Feature feature;
     private Library library;

    public AbstractLibraryFeature() {
    }

    public AbstractLibraryFeature(Feature feature, Library library) {
       this.feature = feature;
       this.library = library;
    }
   
    public Integer getLibraryFeatureId() {
        return this.libraryFeatureId;
    }
    
    public void setLibraryFeatureId(Integer libraryFeatureId) {
        this.libraryFeatureId = libraryFeatureId;
    }
    public Feature getFeature() {
        return this.feature;
    }
    
    public void setFeature(Feature feature) {
        this.feature = feature;
    }
    public Library getLibrary() {
        return this.library;
    }
    
    public void setLibrary(Library library) {
        this.library = library;
    }


   public boolean equals(Object other) {
         if ( (this == other ) ) return true;
		 if ( (other == null ) ) return false;
		 if ( !(other instanceof AbstractLibraryFeature) ) return false;
		 AbstractLibraryFeature castOther = ( AbstractLibraryFeature ) other; 
         
		 return ( (this.getFeature()==castOther.getFeature()) || ( this.getFeature()!=null && castOther.getFeature()!=null && this.getFeature().equals(castOther.getFeature()) ) )
 && ( (this.getLibrary()==castOther.getLibrary()) || ( this.getLibrary()!=null && castOther.getLibrary()!=null && this.getLibrary().equals(castOther.getLibrary()) ) );
   }
   
   public int hashCode() {
         int result = 17;
         
         
         result = 37 * result + ( getFeature() == null ? 0 : this.getFeature().hashCode() );
         result = 37 * result + ( getLibrary() == null ? 0 : this.getLibrary().hashCode() );
         return result;
   }   

public AbstractLibraryFeature generateClone() {
	AbstractLibraryFeature cloned = new LibraryFeature(); 
    	   cloned.feature = this.feature;
    	   cloned.library = this.library;
	return cloned;
}


}


