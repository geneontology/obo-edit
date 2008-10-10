package org.gmod.gbol.simpleObject.generated;


import org.gmod.gbol.simpleObject.*; 

import java.util.HashSet;
import java.util.Set;

/**
 * ExpressionCVTerm generated by hbm2java
 */
public abstract class AbstractExpressionCVTerm extends AbstractSimpleObject implements java.io.Serializable {


     private Integer expressionCVTermId;
     private CVTerm cvterm;
     private CVTerm type;
     private Expression expression;
     private int rank;
     private Set<ExpressionCVTermProperty> expressionCVTermProperties = new HashSet<ExpressionCVTermProperty>(0);

    public AbstractExpressionCVTerm() {
    }

	
    public AbstractExpressionCVTerm(CVTerm cvterm, CVTerm type, Expression expression, int rank) {
        this.cvterm = cvterm;
        this.type = type;
        this.expression = expression;
        this.rank = rank;
    }
    public AbstractExpressionCVTerm(CVTerm cvterm, CVTerm type, Expression expression, int rank, Set<ExpressionCVTermProperty> expressionCVTermProperties) {
       this.cvterm = cvterm;
       this.type = type;
       this.expression = expression;
       this.rank = rank;
       this.expressionCVTermProperties = expressionCVTermProperties;
    }
   
    public Integer getExpressionCVTermId() {
        return this.expressionCVTermId;
    }
    
    public void setExpressionCVTermId(Integer expressionCVTermId) {
        this.expressionCVTermId = expressionCVTermId;
    }
    public CVTerm getCvterm() {
        return this.cvterm;
    }
    
    public void setCvterm(CVTerm cvterm) {
        this.cvterm = cvterm;
    }
    public CVTerm getType() {
        return this.type;
    }
    
    public void setType(CVTerm type) {
        this.type = type;
    }
    public Expression getExpression() {
        return this.expression;
    }
    
    public void setExpression(Expression expression) {
        this.expression = expression;
    }
    public int getRank() {
        return this.rank;
    }
    
    public void setRank(int rank) {
        this.rank = rank;
    }
    public Set<ExpressionCVTermProperty> getExpressionCVTermProperties() {
        return this.expressionCVTermProperties;
    }
    
    public void setExpressionCVTermProperties(Set<ExpressionCVTermProperty> expressionCVTermProperties) {
        this.expressionCVTermProperties = expressionCVTermProperties;
    }


   public boolean equals(Object other) {
         if ( (this == other ) ) return true;
		 if ( (other == null ) ) return false;
		 if ( !(other instanceof AbstractExpressionCVTerm) ) return false;
		 AbstractExpressionCVTerm castOther = ( AbstractExpressionCVTerm ) other; 
         
		 return ( (this.getCvterm()==castOther.getCvterm()) || ( this.getCvterm()!=null && castOther.getCvterm()!=null && this.getCvterm().equals(castOther.getCvterm()) ) )
 && ( (this.getType()==castOther.getType()) || ( this.getType()!=null && castOther.getType()!=null && this.getType().equals(castOther.getType()) ) )
 && ( (this.getExpression()==castOther.getExpression()) || ( this.getExpression()!=null && castOther.getExpression()!=null && this.getExpression().equals(castOther.getExpression()) ) );
   }
   
   public int hashCode() {
         int result = 17;
         
         
         result = 37 * result + ( getCvterm() == null ? 0 : this.getCvterm().hashCode() );
         result = 37 * result + ( getType() == null ? 0 : this.getType().hashCode() );
         result = 37 * result + ( getExpression() == null ? 0 : this.getExpression().hashCode() );
         
         
         return result;
   }   

public AbstractExpressionCVTerm generateClone() {
	AbstractExpressionCVTerm cloned = new ExpressionCVTerm(); 
    	   cloned.cvterm = this.cvterm;
    	   cloned.type = this.type;
    	   cloned.expression = this.expression;
    	   cloned.rank = this.rank;
    	   cloned.expressionCVTermProperties = this.expressionCVTermProperties;
	return cloned;
}


}


