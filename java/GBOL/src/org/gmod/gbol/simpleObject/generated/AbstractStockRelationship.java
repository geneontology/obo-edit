package org.gmod.gbol.simpleObject.generated;


import org.gmod.gbol.simpleObject.*; 

import java.util.HashSet;
import java.util.Set;

/**
 * StockRelationship generated by hbm2java
 */
public abstract class AbstractStockRelationship  implements java.io.Serializable {


     private Integer stockRelationshipId;
     private Stock subjectStock;
     private CVTerm type;
     private Stock objectStock;
     private String value;
     private int rank;
     private Set<StockRelationshipPublication> stockRelationshipPublications = new HashSet<StockRelationshipPublication>(0);

    public AbstractStockRelationship() {
    }

	
    public AbstractStockRelationship(Stock subjectStock, CVTerm type, Stock objectStock, int rank) {
        this.subjectStock = subjectStock;
        this.type = type;
        this.objectStock = objectStock;
        this.rank = rank;
    }
    public AbstractStockRelationship(Stock subjectStock, CVTerm type, Stock objectStock, String value, int rank, Set<StockRelationshipPublication> stockRelationshipPublications) {
       this.subjectStock = subjectStock;
       this.type = type;
       this.objectStock = objectStock;
       this.value = value;
       this.rank = rank;
       this.stockRelationshipPublications = stockRelationshipPublications;
    }
   
    public Integer getStockRelationshipId() {
        return this.stockRelationshipId;
    }
    
    public void setStockRelationshipId(Integer stockRelationshipId) {
        this.stockRelationshipId = stockRelationshipId;
    }
    public Stock getSubjectStock() {
        return this.subjectStock;
    }
    
    public void setSubjectStock(Stock subjectStock) {
        this.subjectStock = subjectStock;
    }
    public CVTerm getType() {
        return this.type;
    }
    
    public void setType(CVTerm type) {
        this.type = type;
    }
    public Stock getObjectStock() {
        return this.objectStock;
    }
    
    public void setObjectStock(Stock objectStock) {
        this.objectStock = objectStock;
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
    public Set<StockRelationshipPublication> getStockRelationshipPublications() {
        return this.stockRelationshipPublications;
    }
    
    public void setStockRelationshipPublications(Set<StockRelationshipPublication> stockRelationshipPublications) {
        this.stockRelationshipPublications = stockRelationshipPublications;
    }


   public boolean equals(Object other) {
         if ( (this == other ) ) return true;
		 if ( (other == null ) ) return false;
		 if ( !(other instanceof AbstractStockRelationship) ) return false;
		 AbstractStockRelationship castOther = ( AbstractStockRelationship ) other; 
         
		 return ( (this.getSubjectStock()==castOther.getSubjectStock()) || ( this.getSubjectStock()!=null && castOther.getSubjectStock()!=null && this.getSubjectStock().equals(castOther.getSubjectStock()) ) )
 && ( (this.getType()==castOther.getType()) || ( this.getType()!=null && castOther.getType()!=null && this.getType().equals(castOther.getType()) ) )
 && ( (this.getObjectStock()==castOther.getObjectStock()) || ( this.getObjectStock()!=null && castOther.getObjectStock()!=null && this.getObjectStock().equals(castOther.getObjectStock()) ) )
 && (this.getRank()==castOther.getRank());
   }
   
   public int hashCode() {
         int result = 17;
         
         
         result = 37 * result + ( getSubjectStock() == null ? 0 : this.getSubjectStock().hashCode() );
         result = 37 * result + ( getType() == null ? 0 : this.getType().hashCode() );
         result = 37 * result + ( getObjectStock() == null ? 0 : this.getObjectStock().hashCode() );
         
         result = 37 * result + this.getRank();
         
         return result;
   }   


}


