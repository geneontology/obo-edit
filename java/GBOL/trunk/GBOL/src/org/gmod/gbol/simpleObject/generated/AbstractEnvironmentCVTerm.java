package org.gmod.gbol.simpleObject.generated;


import org.gmod.gbol.simpleObject.*; 


/**
 * EnvironmentCVTerm generated by hbm2java
 */
public abstract class AbstractEnvironmentCVTerm extends AbstractSimpleObject implements java.io.Serializable {


     private Integer environmentCvtermId;
     private Environment environment;
     private CVTerm cvterm;

    public AbstractEnvironmentCVTerm() {
    }

    public AbstractEnvironmentCVTerm(Environment environment, CVTerm cvterm) {
       this.environment = environment;
       this.cvterm = cvterm;
    }
   
    public Integer getEnvironmentCvtermId() {
        return this.environmentCvtermId;
    }
    
    public void setEnvironmentCvtermId(Integer environmentCvtermId) {
        this.environmentCvtermId = environmentCvtermId;
    }
    public Environment getEnvironment() {
        return this.environment;
    }
    
    public void setEnvironment(Environment environment) {
        this.environment = environment;
    }
    public CVTerm getCvterm() {
        return this.cvterm;
    }
    
    public void setCvterm(CVTerm cvterm) {
        this.cvterm = cvterm;
    }


   public boolean equals(Object other) {
         if ( (this == other ) ) return true;
		 if ( (other == null ) ) return false;
		 if ( !(other instanceof AbstractEnvironmentCVTerm) ) return false;
		 AbstractEnvironmentCVTerm castOther = ( AbstractEnvironmentCVTerm ) other; 
         
		 return ( (this.getEnvironment()==castOther.getEnvironment()) || ( this.getEnvironment()!=null && castOther.getEnvironment()!=null && this.getEnvironment().equals(castOther.getEnvironment()) ) )
 && ( (this.getCvterm()==castOther.getCvterm()) || ( this.getCvterm()!=null && castOther.getCvterm()!=null && this.getCvterm().equals(castOther.getCvterm()) ) );
   }
   
   public int hashCode() {
         int result = 17;
         
         
         result = 37 * result + ( getEnvironment() == null ? 0 : this.getEnvironment().hashCode() );
         result = 37 * result + ( getCvterm() == null ? 0 : this.getCvterm().hashCode() );
         return result;
   }   


}


