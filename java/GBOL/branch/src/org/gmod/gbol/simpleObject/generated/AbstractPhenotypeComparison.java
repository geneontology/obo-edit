package org.gmod.gbol.simpleObject.generated;


import org.gmod.gbol.simpleObject.*; 

import java.util.HashSet;
import java.util.Set;

/**
 * PhenotypeComparison generated by hbm2java
 */
public abstract class AbstractPhenotypeComparison extends AbstractSimpleObject implements java.io.Serializable {


     private Integer phenotypeComparisonId;
     private Phenotype phenotype1;
     private Publication publication;
     private Genotype genotype1;
     private Phenotype phenotype2;
     private Environment environment2;
     private Environment environment1;
     private Genotype genotype2;
     private Organism organism;
     private Set<PhenotypeComparisonCVTerm> phenotypeComparisonCVTerms = new HashSet<PhenotypeComparisonCVTerm>(0);

    public AbstractPhenotypeComparison() {
    }

	
    public AbstractPhenotypeComparison(Phenotype phenotype1, Publication publication, Genotype genotype1, Environment environment2, Environment environment1, Genotype genotype2, Organism organism) {
        this.phenotype1 = phenotype1;
        this.publication = publication;
        this.genotype1 = genotype1;
        this.environment2 = environment2;
        this.environment1 = environment1;
        this.genotype2 = genotype2;
        this.organism = organism;
    }
    public AbstractPhenotypeComparison(Phenotype phenotype1, Publication publication, Genotype genotype1, Phenotype phenotype2, Environment environment2, Environment environment1, Genotype genotype2, Organism organism, Set<PhenotypeComparisonCVTerm> phenotypeComparisonCVTerms) {
       this.phenotype1 = phenotype1;
       this.publication = publication;
       this.genotype1 = genotype1;
       this.phenotype2 = phenotype2;
       this.environment2 = environment2;
       this.environment1 = environment1;
       this.genotype2 = genotype2;
       this.organism = organism;
       this.phenotypeComparisonCVTerms = phenotypeComparisonCVTerms;
    }
   
    public Integer getPhenotypeComparisonId() {
        return this.phenotypeComparisonId;
    }
    
    public void setPhenotypeComparisonId(Integer phenotypeComparisonId) {
        this.phenotypeComparisonId = phenotypeComparisonId;
    }
    public Phenotype getPhenotype1() {
        return this.phenotype1;
    }
    
    public void setPhenotype1(Phenotype phenotype1) {
        this.phenotype1 = phenotype1;
    }
    public Publication getPublication() {
        return this.publication;
    }
    
    public void setPublication(Publication publication) {
        this.publication = publication;
    }
    public Genotype getGenotype1() {
        return this.genotype1;
    }
    
    public void setGenotype1(Genotype genotype1) {
        this.genotype1 = genotype1;
    }
    public Phenotype getPhenotype2() {
        return this.phenotype2;
    }
    
    public void setPhenotype2(Phenotype phenotype2) {
        this.phenotype2 = phenotype2;
    }
    public Environment getEnvironment2() {
        return this.environment2;
    }
    
    public void setEnvironment2(Environment environment2) {
        this.environment2 = environment2;
    }
    public Environment getEnvironment1() {
        return this.environment1;
    }
    
    public void setEnvironment1(Environment environment1) {
        this.environment1 = environment1;
    }
    public Genotype getGenotype2() {
        return this.genotype2;
    }
    
    public void setGenotype2(Genotype genotype2) {
        this.genotype2 = genotype2;
    }
    public Organism getOrganism() {
        return this.organism;
    }
    
    public void setOrganism(Organism organism) {
        this.organism = organism;
    }
    public Set<PhenotypeComparisonCVTerm> getPhenotypeComparisonCVTerms() {
        return this.phenotypeComparisonCVTerms;
    }
    
    public void setPhenotypeComparisonCVTerms(Set<PhenotypeComparisonCVTerm> phenotypeComparisonCVTerms) {
        this.phenotypeComparisonCVTerms = phenotypeComparisonCVTerms;
    }


   public boolean equals(Object other) {
         if ( (this == other ) ) return true;
		 if ( (other == null ) ) return false;
		 if ( !(other instanceof AbstractPhenotypeComparison) ) return false;
		 AbstractPhenotypeComparison castOther = ( AbstractPhenotypeComparison ) other; 
         
		 return ( (this.getPhenotype1()==castOther.getPhenotype1()) || ( this.getPhenotype1()!=null && castOther.getPhenotype1()!=null && this.getPhenotype1().equals(castOther.getPhenotype1()) ) )
 && ( (this.getPublication()==castOther.getPublication()) || ( this.getPublication()!=null && castOther.getPublication()!=null && this.getPublication().equals(castOther.getPublication()) ) )
 && ( (this.getGenotype1()==castOther.getGenotype1()) || ( this.getGenotype1()!=null && castOther.getGenotype1()!=null && this.getGenotype1().equals(castOther.getGenotype1()) ) )
 && ( (this.getEnvironment2()==castOther.getEnvironment2()) || ( this.getEnvironment2()!=null && castOther.getEnvironment2()!=null && this.getEnvironment2().equals(castOther.getEnvironment2()) ) )
 && ( (this.getEnvironment1()==castOther.getEnvironment1()) || ( this.getEnvironment1()!=null && castOther.getEnvironment1()!=null && this.getEnvironment1().equals(castOther.getEnvironment1()) ) )
 && ( (this.getGenotype2()==castOther.getGenotype2()) || ( this.getGenotype2()!=null && castOther.getGenotype2()!=null && this.getGenotype2().equals(castOther.getGenotype2()) ) );
   }
   
   public int hashCode() {
         int result = 17;
         
         
         result = 37 * result + ( getPhenotype1() == null ? 0 : this.getPhenotype1().hashCode() );
         result = 37 * result + ( getPublication() == null ? 0 : this.getPublication().hashCode() );
         result = 37 * result + ( getGenotype1() == null ? 0 : this.getGenotype1().hashCode() );
         
         result = 37 * result + ( getEnvironment2() == null ? 0 : this.getEnvironment2().hashCode() );
         result = 37 * result + ( getEnvironment1() == null ? 0 : this.getEnvironment1().hashCode() );
         result = 37 * result + ( getGenotype2() == null ? 0 : this.getGenotype2().hashCode() );
         
         
         return result;
   }   

public AbstractPhenotypeComparison generateClone() {
	AbstractPhenotypeComparison cloned = new PhenotypeComparison(); 
    	   cloned.phenotype1 = this.phenotype1;
    	   cloned.publication = this.publication;
    	   cloned.genotype1 = this.genotype1;
    	   cloned.phenotype2 = this.phenotype2;
    	   cloned.environment2 = this.environment2;
    	   cloned.environment1 = this.environment1;
    	   cloned.genotype2 = this.genotype2;
    	   cloned.organism = this.organism;
    	   cloned.phenotypeComparisonCVTerms = this.phenotypeComparisonCVTerms;
	return cloned;
}


}


