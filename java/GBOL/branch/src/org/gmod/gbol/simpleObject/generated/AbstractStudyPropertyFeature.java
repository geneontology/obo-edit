package org.gmod.gbol.simpleObject.generated;


import org.gmod.gbol.simpleObject.*; 


/**
 * StudyPropertyFeature generated by hbm2java
 */
public abstract class AbstractStudyPropertyFeature extends AbstractSimpleObject implements java.io.Serializable {


     private Integer studyPropertyFeatureId;
     private CVTerm type;
     private Feature feature;
     private StudyProperty studyProperty;

    public AbstractStudyPropertyFeature() {
    }

	
    public AbstractStudyPropertyFeature(Feature feature, StudyProperty studyProperty) {
        this.feature = feature;
        this.studyProperty = studyProperty;
    }
    public AbstractStudyPropertyFeature(CVTerm type, Feature feature, StudyProperty studyProperty) {
       this.type = type;
       this.feature = feature;
       this.studyProperty = studyProperty;
    }
   
    public Integer getStudyPropertyFeatureId() {
        return this.studyPropertyFeatureId;
    }
    
    public void setStudyPropertyFeatureId(Integer studyPropertyFeatureId) {
        this.studyPropertyFeatureId = studyPropertyFeatureId;
    }
    public CVTerm getType() {
        return this.type;
    }
    
    public void setType(CVTerm type) {
        this.type = type;
    }
    public Feature getFeature() {
        return this.feature;
    }
    
    public void setFeature(Feature feature) {
        this.feature = feature;
    }
    public StudyProperty getStudyProperty() {
        return this.studyProperty;
    }
    
    public void setStudyProperty(StudyProperty studyProperty) {
        this.studyProperty = studyProperty;
    }


   public boolean equals(Object other) {
         if ( (this == other ) ) return true;
		 if ( (other == null ) ) return false;
		 if ( !(other instanceof AbstractStudyPropertyFeature) ) return false;
		 AbstractStudyPropertyFeature castOther = ( AbstractStudyPropertyFeature ) other; 
         
		 return ( (this.getFeature()==castOther.getFeature()) || ( this.getFeature()!=null && castOther.getFeature()!=null && this.getFeature().equals(castOther.getFeature()) ) )
 && ( (this.getStudyProperty()==castOther.getStudyProperty()) || ( this.getStudyProperty()!=null && castOther.getStudyProperty()!=null && this.getStudyProperty().equals(castOther.getStudyProperty()) ) );
   }
   
   public int hashCode() {
         int result = 17;
         
         
         
         result = 37 * result + ( getFeature() == null ? 0 : this.getFeature().hashCode() );
         result = 37 * result + ( getStudyProperty() == null ? 0 : this.getStudyProperty().hashCode() );
         return result;
   }   

public AbstractStudyPropertyFeature generateClone() {
	AbstractStudyPropertyFeature cloned = new StudyPropertyFeature(); 
    	   cloned.type = this.type;
    	   cloned.feature = this.feature;
    	   cloned.studyProperty = this.studyProperty;
	return cloned;
}


}


