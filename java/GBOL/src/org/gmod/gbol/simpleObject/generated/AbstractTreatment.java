package org.gmod.gbol.simpleObject.generated;


import org.gmod.gbol.simpleObject.*; 

import java.util.HashSet;
import java.util.Set;

/**
 * Treatment generated by hbm2java
 */
public abstract class AbstractTreatment  implements java.io.Serializable {


     private Integer treatmentId;
     private CVTerm type;
     private Protocol protocol;
     private Biomaterial biomaterial;
     private int rank;
     private String name;
     private Set<BiomaterialTreatment> biomaterialTreatments = new HashSet<BiomaterialTreatment>(0);

    public AbstractTreatment() {
    }

	
    public AbstractTreatment(CVTerm type, Biomaterial biomaterial, int rank) {
        this.type = type;
        this.biomaterial = biomaterial;
        this.rank = rank;
    }
    public AbstractTreatment(CVTerm type, Protocol protocol, Biomaterial biomaterial, int rank, String name, Set<BiomaterialTreatment> biomaterialTreatments) {
       this.type = type;
       this.protocol = protocol;
       this.biomaterial = biomaterial;
       this.rank = rank;
       this.name = name;
       this.biomaterialTreatments = biomaterialTreatments;
    }
   
    public Integer getTreatmentId() {
        return this.treatmentId;
    }
    
    public void setTreatmentId(Integer treatmentId) {
        this.treatmentId = treatmentId;
    }
    public CVTerm getType() {
        return this.type;
    }
    
    public void setType(CVTerm type) {
        this.type = type;
    }
    public Protocol getProtocol() {
        return this.protocol;
    }
    
    public void setProtocol(Protocol protocol) {
        this.protocol = protocol;
    }
    public Biomaterial getBiomaterial() {
        return this.biomaterial;
    }
    
    public void setBiomaterial(Biomaterial biomaterial) {
        this.biomaterial = biomaterial;
    }
    public int getRank() {
        return this.rank;
    }
    
    public void setRank(int rank) {
        this.rank = rank;
    }
    public String getName() {
        return this.name;
    }
    
    public void setName(String name) {
        this.name = name;
    }
    public Set<BiomaterialTreatment> getBiomaterialTreatments() {
        return this.biomaterialTreatments;
    }
    
    public void setBiomaterialTreatments(Set<BiomaterialTreatment> biomaterialTreatments) {
        this.biomaterialTreatments = biomaterialTreatments;
    }


   public boolean equals(Object other) {
         if ( (this == other ) ) return true;
		 if ( (other == null ) ) return false;
		 if ( !(other instanceof AbstractTreatment) ) return false;
		 AbstractTreatment castOther = ( AbstractTreatment ) other; 
         
		 return ( (this.getType()==castOther.getType()) || ( this.getType()!=null && castOther.getType()!=null && this.getType().equals(castOther.getType()) ) )
 && ( (this.getProtocol()==castOther.getProtocol()) || ( this.getProtocol()!=null && castOther.getProtocol()!=null && this.getProtocol().equals(castOther.getProtocol()) ) )
 && ( (this.getBiomaterial()==castOther.getBiomaterial()) || ( this.getBiomaterial()!=null && castOther.getBiomaterial()!=null && this.getBiomaterial().equals(castOther.getBiomaterial()) ) );
   }
   
   public int hashCode() {
         int result = 17;
         
         
         result = 37 * result + ( getType() == null ? 0 : this.getType().hashCode() );
         result = 37 * result + ( getProtocol() == null ? 0 : this.getProtocol().hashCode() );
         result = 37 * result + ( getBiomaterial() == null ? 0 : this.getBiomaterial().hashCode() );
         
         
         
         return result;
   }   


}


