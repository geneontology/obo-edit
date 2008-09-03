package org.gmod.gbol.simpleObject.generated;


import org.gmod.gbol.simpleObject.*; 


/**
 * QuantificationRelationship generated by hbm2java
 */
public abstract class AbstractQuantificationRelationship  implements java.io.Serializable {


     private Integer quantificationRelationshipId;
     private Quantification subjectQuantification;
     private CVTerm type;
     private Quantification objectQuantification;

    public AbstractQuantificationRelationship() {
    }

    public AbstractQuantificationRelationship(Quantification subjectQuantification, CVTerm type, Quantification objectQuantification) {
       this.subjectQuantification = subjectQuantification;
       this.type = type;
       this.objectQuantification = objectQuantification;
    }
   
    public Integer getQuantificationRelationshipId() {
        return this.quantificationRelationshipId;
    }
    
    public void setQuantificationRelationshipId(Integer quantificationRelationshipId) {
        this.quantificationRelationshipId = quantificationRelationshipId;
    }
    public Quantification getSubjectQuantification() {
        return this.subjectQuantification;
    }
    
    public void setSubjectQuantification(Quantification subjectQuantification) {
        this.subjectQuantification = subjectQuantification;
    }
    public CVTerm getType() {
        return this.type;
    }
    
    public void setType(CVTerm type) {
        this.type = type;
    }
    public Quantification getObjectQuantification() {
        return this.objectQuantification;
    }
    
    public void setObjectQuantification(Quantification objectQuantification) {
        this.objectQuantification = objectQuantification;
    }


   public boolean equals(Object other) {
         if ( (this == other ) ) return true;
		 if ( (other == null ) ) return false;
		 if ( !(other instanceof AbstractQuantificationRelationship) ) return false;
		 AbstractQuantificationRelationship castOther = ( AbstractQuantificationRelationship ) other; 
         
		 return ( (this.getSubjectQuantification()==castOther.getSubjectQuantification()) || ( this.getSubjectQuantification()!=null && castOther.getSubjectQuantification()!=null && this.getSubjectQuantification().equals(castOther.getSubjectQuantification()) ) )
 && ( (this.getObjectQuantification()==castOther.getObjectQuantification()) || ( this.getObjectQuantification()!=null && castOther.getObjectQuantification()!=null && this.getObjectQuantification().equals(castOther.getObjectQuantification()) ) );
   }
   
   public int hashCode() {
         int result = 17;
         
         
         result = 37 * result + ( getSubjectQuantification() == null ? 0 : this.getSubjectQuantification().hashCode() );
         
         result = 37 * result + ( getObjectQuantification() == null ? 0 : this.getObjectQuantification().hashCode() );
         return result;
   }   


}


