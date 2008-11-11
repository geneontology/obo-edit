package org.gmod.gbol.simpleObject.generated;


import org.gmod.gbol.simpleObject.*; 


/**
 * PublicationRelationship generated by hbm2java
 */
public abstract class AbstractPublicationRelationship extends AbstractSimpleObject implements java.io.Serializable {


     private Integer publicationRelationshipId;
     private Publication subjectPublication;
     private CVTerm type;
     private Publication objectPublication;

    public AbstractPublicationRelationship() {
    }

    public AbstractPublicationRelationship(Publication subjectPublication, CVTerm type, Publication objectPublication) {
       this.subjectPublication = subjectPublication;
       this.type = type;
       this.objectPublication = objectPublication;
    }
   
    public Integer getPublicationRelationshipId() {
        return this.publicationRelationshipId;
    }
    
    public void setPublicationRelationshipId(Integer publicationRelationshipId) {
        this.publicationRelationshipId = publicationRelationshipId;
    }
    public Publication getSubjectPublication() {
        return this.subjectPublication;
    }
    
    public void setSubjectPublication(Publication subjectPublication) {
        this.subjectPublication = subjectPublication;
    }
    public CVTerm getType() {
        return this.type;
    }
    
    public void setType(CVTerm type) {
        this.type = type;
    }
    public Publication getObjectPublication() {
        return this.objectPublication;
    }
    
    public void setObjectPublication(Publication objectPublication) {
        this.objectPublication = objectPublication;
    }


   public boolean equals(Object other) {
         if ( (this == other ) ) return true;
		 if ( (other == null ) ) return false;
		 if ( !(other instanceof AbstractPublicationRelationship) ) return false;
		 AbstractPublicationRelationship castOther = ( AbstractPublicationRelationship ) other; 
         
		 return ( (this.getSubjectPublication()==castOther.getSubjectPublication()) || ( this.getSubjectPublication()!=null && castOther.getSubjectPublication()!=null && this.getSubjectPublication().equals(castOther.getSubjectPublication()) ) )
 && ( (this.getType()==castOther.getType()) || ( this.getType()!=null && castOther.getType()!=null && this.getType().equals(castOther.getType()) ) )
 && ( (this.getObjectPublication()==castOther.getObjectPublication()) || ( this.getObjectPublication()!=null && castOther.getObjectPublication()!=null && this.getObjectPublication().equals(castOther.getObjectPublication()) ) );
   }
   
   public int hashCode() {
         int result = 17;
         
         
         result = 37 * result + ( getSubjectPublication() == null ? 0 : this.getSubjectPublication().hashCode() );
         result = 37 * result + ( getType() == null ? 0 : this.getType().hashCode() );
         result = 37 * result + ( getObjectPublication() == null ? 0 : this.getObjectPublication().hashCode() );
         return result;
   }   


}


