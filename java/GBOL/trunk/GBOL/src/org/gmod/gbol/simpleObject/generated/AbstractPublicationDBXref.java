package org.gmod.gbol.simpleObject.generated;


import org.gmod.gbol.simpleObject.*; 


/**
 * PublicationDBXref generated by hbm2java
 */
public abstract class AbstractPublicationDBXref extends AbstractSimpleObject implements java.io.Serializable {


     private Integer publicationDBXrefId;
     private Publication publication;
     private DBXref dbxref;
     private boolean isCurrent;

    public AbstractPublicationDBXref() {
    }

    public AbstractPublicationDBXref(Publication publication, DBXref dbxref, boolean isCurrent) {
       this.publication = publication;
       this.dbxref = dbxref;
       this.isCurrent = isCurrent;
    }
   
    public Integer getPublicationDBXrefId() {
        return this.publicationDBXrefId;
    }
    
    public void setPublicationDBXrefId(Integer publicationDBXrefId) {
        this.publicationDBXrefId = publicationDBXrefId;
    }
    public Publication getPublication() {
        return this.publication;
    }
    
    public void setPublication(Publication publication) {
        this.publication = publication;
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
		 if ( !(other instanceof AbstractPublicationDBXref) ) return false;
		 AbstractPublicationDBXref castOther = ( AbstractPublicationDBXref ) other; 
         
		 return ( (this.getPublication()==castOther.getPublication()) || ( this.getPublication()!=null && castOther.getPublication()!=null && this.getPublication().equals(castOther.getPublication()) ) )
 && ( (this.getDbxref()==castOther.getDbxref()) || ( this.getDbxref()!=null && castOther.getDbxref()!=null && this.getDbxref().equals(castOther.getDbxref()) ) );
   }
   
   public int hashCode() {
         int result = 17;
         
         
         result = 37 * result + ( getPublication() == null ? 0 : this.getPublication().hashCode() );
         result = 37 * result + ( getDbxref() == null ? 0 : this.getDbxref().hashCode() );
         
         return result;
   }   


}


