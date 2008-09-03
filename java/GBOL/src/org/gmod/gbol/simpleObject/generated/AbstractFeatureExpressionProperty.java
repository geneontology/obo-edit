package org.gmod.gbol.simpleObject.generated;


import org.gmod.gbol.simpleObject.*; 


/**
 * FeatureExpressionProperty generated by hbm2java
 */
public abstract class AbstractFeatureExpressionProperty  implements java.io.Serializable {


     private Integer featureExpressionPropertyId;
     private CVTerm type;
     private FeatureExpression featureExpression;
     private String value;
     private int rank;

    public AbstractFeatureExpressionProperty() {
    }

	
    public AbstractFeatureExpressionProperty(CVTerm type, FeatureExpression featureExpression, int rank) {
        this.type = type;
        this.featureExpression = featureExpression;
        this.rank = rank;
    }
    public AbstractFeatureExpressionProperty(CVTerm type, FeatureExpression featureExpression, String value, int rank) {
       this.type = type;
       this.featureExpression = featureExpression;
       this.value = value;
       this.rank = rank;
    }
   
    public Integer getFeatureExpressionPropertyId() {
        return this.featureExpressionPropertyId;
    }
    
    public void setFeatureExpressionPropertyId(Integer featureExpressionPropertyId) {
        this.featureExpressionPropertyId = featureExpressionPropertyId;
    }
    public CVTerm getType() {
        return this.type;
    }
    
    public void setType(CVTerm type) {
        this.type = type;
    }
    public FeatureExpression getFeatureExpression() {
        return this.featureExpression;
    }
    
    public void setFeatureExpression(FeatureExpression featureExpression) {
        this.featureExpression = featureExpression;
    }
    public String getValue() {
        return this.value;
    }
    
    public void setValue(String value) {
        this.value = value;
    }
    public int getRank() {
        return this.rank;
    }
    
    public void setRank(int rank) {
        this.rank = rank;
    }


   public boolean equals(Object other) {
         if ( (this == other ) ) return true;
		 if ( (other == null ) ) return false;
		 if ( !(other instanceof AbstractFeatureExpressionProperty) ) return false;
		 AbstractFeatureExpressionProperty castOther = ( AbstractFeatureExpressionProperty ) other; 
         
		 return ( (this.getType()==castOther.getType()) || ( this.getType()!=null && castOther.getType()!=null && this.getType().equals(castOther.getType()) ) )
 && ( (this.getFeatureExpression()==castOther.getFeatureExpression()) || ( this.getFeatureExpression()!=null && castOther.getFeatureExpression()!=null && this.getFeatureExpression().equals(castOther.getFeatureExpression()) ) )
 && (this.getRank()==castOther.getRank());
   }
   
   public int hashCode() {
         int result = 17;
         
         
         result = 37 * result + ( getType() == null ? 0 : this.getType().hashCode() );
         result = 37 * result + ( getFeatureExpression() == null ? 0 : this.getFeatureExpression().hashCode() );
         
         result = 37 * result + this.getRank();
         return result;
   }   


}


