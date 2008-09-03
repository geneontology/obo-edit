package org.gmod.gbol.simpleObject.generated;


import org.gmod.gbol.simpleObject.*; 


/**
 * FeatureMapPublication generated by hbm2java
 */
public abstract class AbstractFeatureMapPublication  implements java.io.Serializable {


     private Integer featureMapPublicationId;
     private Publication publication;
     private FeatureMap featureMap;

    public AbstractFeatureMapPublication() {
    }

    public AbstractFeatureMapPublication(Publication publication, FeatureMap featureMap) {
       this.publication = publication;
       this.featureMap = featureMap;
    }
   
    public Integer getFeatureMapPublicationId() {
        return this.featureMapPublicationId;
    }
    
    public void setFeatureMapPublicationId(Integer featureMapPublicationId) {
        this.featureMapPublicationId = featureMapPublicationId;
    }
    public Publication getPublication() {
        return this.publication;
    }
    
    public void setPublication(Publication publication) {
        this.publication = publication;
    }
    public FeatureMap getFeatureMap() {
        return this.featureMap;
    }
    
    public void setFeatureMap(FeatureMap featureMap) {
        this.featureMap = featureMap;
    }


   public boolean equals(Object other) {
         if ( (this == other ) ) return true;
		 if ( (other == null ) ) return false;
		 if ( !(other instanceof AbstractFeatureMapPublication) ) return false;
		 AbstractFeatureMapPublication castOther = ( AbstractFeatureMapPublication ) other; 
         
		 return ( (this.getPublication()==castOther.getPublication()) || ( this.getPublication()!=null && castOther.getPublication()!=null && this.getPublication().equals(castOther.getPublication()) ) )
 && ( (this.getFeatureMap()==castOther.getFeatureMap()) || ( this.getFeatureMap()!=null && castOther.getFeatureMap()!=null && this.getFeatureMap().equals(castOther.getFeatureMap()) ) );
   }
   
   public int hashCode() {
         int result = 17;
         
         
         result = 37 * result + ( getPublication() == null ? 0 : this.getPublication().hashCode() );
         result = 37 * result + ( getFeatureMap() == null ? 0 : this.getFeatureMap().hashCode() );
         return result;
   }   


}


