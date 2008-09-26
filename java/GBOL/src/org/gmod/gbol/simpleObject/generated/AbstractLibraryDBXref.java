package org.gmod.gbol.simpleObject.generated;


import org.gmod.gbol.simpleObject.*; 


/**
 * LibraryDBXref generated by hbm2java
 */
public abstract class AbstractLibraryDBXref extends AbstractSimpleObject implements java.io.Serializable {


     private Integer libraryDBXrefId;
     private Library library;
     private DBXref dbxref;
     private boolean isCurrent;

    public AbstractLibraryDBXref() {
    }

    public AbstractLibraryDBXref(Library library, DBXref dbxref, boolean isCurrent) {
       this.library = library;
       this.dbxref = dbxref;
       this.isCurrent = isCurrent;
    }
   
    public Integer getLibraryDBXrefId() {
        return this.libraryDBXrefId;
    }
    
    public void setLibraryDBXrefId(Integer libraryDBXrefId) {
        this.libraryDBXrefId = libraryDBXrefId;
    }
    public Library getLibrary() {
        return this.library;
    }
    
    public void setLibrary(Library library) {
        this.library = library;
    }
    public DBXref getDbxref() {
        return this.dbxref;
    }
    
    public void setDbxref(DBXref dbxref) {
        this.dbxref = dbxref;
    }
    public boolean isIsCurrent() {
        return this.isCurrent;
    }
    
    public void setIsCurrent(boolean isCurrent) {
        this.isCurrent = isCurrent;
    }


   public boolean equals(Object other) {
         if ( (this == other ) ) return true;
		 if ( (other == null ) ) return false;
		 if ( !(other instanceof AbstractLibraryDBXref) ) return false;
		 AbstractLibraryDBXref castOther = ( AbstractLibraryDBXref ) other; 
         
		 return ( (this.getLibrary()==castOther.getLibrary()) || ( this.getLibrary()!=null && castOther.getLibrary()!=null && this.getLibrary().equals(castOther.getLibrary()) ) )
 && ( (this.getDbxref()==castOther.getDbxref()) || ( this.getDbxref()!=null && castOther.getDbxref()!=null && this.getDbxref().equals(castOther.getDbxref()) ) );
   }
   
   public int hashCode() {
         int result = 17;
         
         
         result = 37 * result + ( getLibrary() == null ? 0 : this.getLibrary().hashCode() );
         result = 37 * result + ( getDbxref() == null ? 0 : this.getDbxref().hashCode() );
         
         return result;
   }   


}


