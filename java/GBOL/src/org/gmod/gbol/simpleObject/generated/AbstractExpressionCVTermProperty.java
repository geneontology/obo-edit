package org.gmod.gbol.simpleObject.generated;


import org.gmod.gbol.simpleObject.*; 


/**
 * ExpressionCVTermProperty generated by hbm2java
 */
public abstract class AbstractExpressionCVTermProperty  implements java.io.Serializable {


     private Integer expressionCVTermPropertyId;
     private CVTerm type;
     private ExpressionCVTerm expressionCVTerm;
     private String value;
     private int rank;

    public AbstractExpressionCVTermProperty() {
    }

	
    public AbstractExpressionCVTermProperty(CVTerm type, ExpressionCVTerm expressionCVTerm, int rank) {
        this.type = type;
        this.expressionCVTerm = expressionCVTerm;
        this.rank = rank;
    }
    public AbstractExpressionCVTermProperty(CVTerm type, ExpressionCVTerm expressionCVTerm, String value, int rank) {
       this.type = type;
       this.expressionCVTerm = expressionCVTerm;
       this.value = value;
       this.rank = rank;
    }
   
    public Integer getExpressionCVTermPropertyId() {
        return this.expressionCVTermPropertyId;
    }
    
    public void setExpressionCVTermPropertyId(Integer expressionCVTermPropertyId) {
        this.expressionCVTermPropertyId = expressionCVTermPropertyId;
    }
    public CVTerm getType() {
        return this.type;
    }
    
    public void setType(CVTerm type) {
        this.type = type;
    }
    public ExpressionCVTerm getExpressionCVTerm() {
        return this.expressionCVTerm;
    }
    
    public void setExpressionCVTerm(ExpressionCVTerm expressionCVTerm) {
        this.expressionCVTerm = expressionCVTerm;
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
		 if ( !(other instanceof AbstractExpressionCVTermProperty) ) return false;
		 AbstractExpressionCVTermProperty castOther = ( AbstractExpressionCVTermProperty ) other; 
         
		 return ( (this.getType()==castOther.getType()) || ( this.getType()!=null && castOther.getType()!=null && this.getType().equals(castOther.getType()) ) )
 && ( (this.getExpressionCVTerm()==castOther.getExpressionCVTerm()) || ( this.getExpressionCVTerm()!=null && castOther.getExpressionCVTerm()!=null && this.getExpressionCVTerm().equals(castOther.getExpressionCVTerm()) ) )
 && (this.getRank()==castOther.getRank());
   }
   
   public int hashCode() {
         int result = 17;
         
         
         result = 37 * result + ( getType() == null ? 0 : this.getType().hashCode() );
         result = 37 * result + ( getExpressionCVTerm() == null ? 0 : this.getExpressionCVTerm().hashCode() );
         
         result = 37 * result + this.getRank();
         return result;
   }   


}


