package org.gmod.gbol.simpleObject.generated;


import org.gmod.gbol.simpleObject.*; 


/**
 * LibraryPublication generated by hbm2java
 */
public abstract class AbstractLibraryPublication extends AbstractSimpleObject implements java.io.Serializable {


     private Integer libraryPublicationId;
     private Publication publication;
     private Library library;

    public AbstractLibraryPublication() {
    }

    public AbstractLibraryPublication(Publication publication, Library library) {
       this.publication = publication;
       this.library = library;
    }
   
    public Integer getLibraryPublicationId() {
        return this.libraryPublicationId;
    }
    
    public void setLibraryPublicationId(Integer libraryPublicationId) {
        this.libraryPublicationId = libraryPublicationId;
    }
    public Publication getPublication() {
        return this.publication;
    }
    
    public void setPublication(Publication publication) {
        this.publication = publication;
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
		 if ( !(other instanceof AbstractLibraryPublication) ) return false;
		 AbstractLibraryPublication castOther = ( AbstractLibraryPublication ) other; 
         
		 return ( (this.getPublication()==castOther.getPublication()) || ( this.getPublication()!=null && castOther.getPublication()!=null && this.getPublication().equals(castOther.getPublication()) ) )
 && ( (this.getLibrary()==castOther.getLibrary()) || ( this.getLibrary()!=null && castOther.getLibrary()!=null && this.getLibrary().equals(castOther.getLibrary()) ) );
   }
   
   public int hashCode() {
         int result = 17;
         
         
         result = 37 * result + ( getPublication() == null ? 0 : this.getPublication().hashCode() );
         result = 37 * result + ( getLibrary() == null ? 0 : this.getLibrary().hashCode() );
         return result;
   }   


}


