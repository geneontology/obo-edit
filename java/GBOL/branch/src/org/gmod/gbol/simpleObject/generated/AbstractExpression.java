package org.gmod.gbol.simpleObject.generated;


import org.gmod.gbol.simpleObject.*; 

import java.util.HashSet;
import java.util.Set;

/**
 * Expression generated by hbm2java
 */
public abstract class AbstractExpression extends AbstractSimpleObject implements java.io.Serializable {


     private Integer expressionId;
     private String uniquename;
     private String md5checksum;
     private String description;
     private Set<ExpressionProperty> expressionProperties = new HashSet<ExpressionProperty>(0);
     private Set<ExpressionPublication> expressionPublications = new HashSet<ExpressionPublication>(0);
     private Set<FeatureExpression> featureExpressions = new HashSet<FeatureExpression>(0);
     private Set<ExpressionImage> expressionImages = new HashSet<ExpressionImage>(0);
     private Set<ExpressionCVTerm> expressionCVTerms = new HashSet<ExpressionCVTerm>(0);

    public AbstractExpression() {
    }

	
    public AbstractExpression(String uniquename) {
        this.uniquename = uniquename;
    }
    public AbstractExpression(String uniquename, String md5checksum, String description, Set<ExpressionProperty> expressionProperties, Set<ExpressionPublication> expressionPublications, Set<FeatureExpression> featureExpressions, Set<ExpressionImage> expressionImages, Set<ExpressionCVTerm> expressionCVTerms) {
       this.uniquename = uniquename;
       this.md5checksum = md5checksum;
       this.description = description;
       this.expressionProperties = expressionProperties;
       this.expressionPublications = expressionPublications;
       this.featureExpressions = featureExpressions;
       this.expressionImages = expressionImages;
       this.expressionCVTerms = expressionCVTerms;
    }
   
    public Integer getExpressionId() {
        return this.expressionId;
    }
    
    public void setExpressionId(Integer expressionId) {
        this.expressionId = expressionId;
    }
    public String getUniquename() {
        return this.uniquename;
    }
    
    public void setUniquename(String uniquename) {
        this.uniquename = uniquename;
    }
    public String getMd5checksum() {
        return this.md5checksum;
    }
    
    public void setMd5checksum(String md5checksum) {
        this.md5checksum = md5checksum;
    }
    public String getDescription() {
        return this.description;
    }
    
    public void setDescription(String description) {
        this.description = description;
    }
    public Set<ExpressionProperty> getExpressionProperties() {
        return this.expressionProperties;
    }
    
    public void setExpressionProperties(Set<ExpressionProperty> expressionProperties) {
        this.expressionProperties = expressionProperties;
    }
    public Set<ExpressionPublication> getExpressionPublications() {
        return this.expressionPublications;
    }
    
    public void setExpressionPublications(Set<ExpressionPublication> expressionPublications) {
        this.expressionPublications = expressionPublications;
    }
    public Set<FeatureExpression> getFeatureExpressions() {
        return this.featureExpressions;
    }
    
    public void setFeatureExpressions(Set<FeatureExpression> featureExpressions) {
        this.featureExpressions = featureExpressions;
    }
    public Set<ExpressionImage> getExpressionImages() {
        return this.expressionImages;
    }
    
    public void setExpressionImages(Set<ExpressionImage> expressionImages) {
        this.expressionImages = expressionImages;
    }
    public Set<ExpressionCVTerm> getExpressionCVTerms() {
        return this.expressionCVTerms;
    }
    
    public void setExpressionCVTerms(Set<ExpressionCVTerm> expressionCVTerms) {
        this.expressionCVTerms = expressionCVTerms;
    }


   public boolean equals(Object other) {
         if ( (this == other ) ) return true;
		 if ( (other == null ) ) return false;
		 if ( !(other instanceof AbstractExpression) ) return false;
		 AbstractExpression castOther = ( AbstractExpression ) other; 
         
		 return ( (this.getUniquename()==castOther.getUniquename()) || ( this.getUniquename()!=null && castOther.getUniquename()!=null && this.getUniquename().equals(castOther.getUniquename()) ) );
   }
   
   public int hashCode() {
         int result = 17;
         
         
         result = 37 * result + ( getUniquename() == null ? 0 : this.getUniquename().hashCode() );
         
         
         
         
         
         
         
         return result;
   }   

public AbstractExpression generateClone() {
	AbstractExpression cloned = new Expression(); 
    	   cloned.uniquename = this.uniquename;
    	   cloned.md5checksum = this.md5checksum;
    	   cloned.description = this.description;
    	   cloned.expressionProperties = this.expressionProperties;
    	   cloned.expressionPublications = this.expressionPublications;
    	   cloned.featureExpressions = this.featureExpressions;
    	   cloned.expressionImages = this.expressionImages;
    	   cloned.expressionCVTerms = this.expressionCVTerms;
	return cloned;
}


}


