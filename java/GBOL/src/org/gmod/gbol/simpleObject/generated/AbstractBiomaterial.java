package org.gmod.gbol.simpleObject.generated;


import org.gmod.gbol.simpleObject.*; 

import java.util.HashSet;
import java.util.Set;

/**
 * Biomaterial generated by hbm2java
 */
public abstract class AbstractBiomaterial  implements java.io.Serializable {


     private Integer biomaterialId;
     private Organism taxon;
     private Contact sourceProvider;
     private DBXref dbxref;
     private String name;
     private String description;
     private Set<BiomaterialProperty> biomaterialProperties = new HashSet<BiomaterialProperty>(0);
     private Set<BiomaterialDBXref> biomaterialDBXrefs = new HashSet<BiomaterialDBXref>(0);
     private Set<Treatment> treatments = new HashSet<Treatment>(0);
     private Set<BiomaterialRelationship> parentBiomaterialRelationships = new HashSet<BiomaterialRelationship>(0);
     private Set<AssayBiomaterial> assayBiomaterials = new HashSet<AssayBiomaterial>(0);
     private Set<BiomaterialRelationship> childBiomaterialRelationships = new HashSet<BiomaterialRelationship>(0);
     private Set<BiomaterialTreatment> biomaterialTreatments = new HashSet<BiomaterialTreatment>(0);

    public AbstractBiomaterial() {
    }

    public AbstractBiomaterial(Organism taxon, Contact sourceProvider, DBXref dbxref, String name, String description, Set<BiomaterialProperty> biomaterialProperties, Set<BiomaterialDBXref> biomaterialDBXrefs, Set<Treatment> treatments, Set<BiomaterialRelationship> parentBiomaterialRelationships, Set<AssayBiomaterial> assayBiomaterials, Set<BiomaterialRelationship> childBiomaterialRelationships, Set<BiomaterialTreatment> biomaterialTreatments) {
       this.taxon = taxon;
       this.sourceProvider = sourceProvider;
       this.dbxref = dbxref;
       this.name = name;
       this.description = description;
       this.biomaterialProperties = biomaterialProperties;
       this.biomaterialDBXrefs = biomaterialDBXrefs;
       this.treatments = treatments;
       this.parentBiomaterialRelationships = parentBiomaterialRelationships;
       this.assayBiomaterials = assayBiomaterials;
       this.childBiomaterialRelationships = childBiomaterialRelationships;
       this.biomaterialTreatments = biomaterialTreatments;
    }
   
    public Integer getBiomaterialId() {
        return this.biomaterialId;
    }
    
    public void setBiomaterialId(Integer biomaterialId) {
        this.biomaterialId = biomaterialId;
    }
    public Organism getTaxon() {
        return this.taxon;
    }
    
    public void setTaxon(Organism taxon) {
        this.taxon = taxon;
    }
    public Contact getSourceProvider() {
        return this.sourceProvider;
    }
    
    public void setSourceProvider(Contact sourceProvider) {
        this.sourceProvider = sourceProvider;
    }
    public DBXref getDbxref() {
        return this.dbxref;
    }
    
    public void setDbxref(DBXref dbxref) {
        this.dbxref = dbxref;
    }
    public String getName() {
        return this.name;
    }
    
    public void setName(String name) {
        this.name = name;
    }
    public String getDescription() {
        return this.description;
    }
    
    public void setDescription(String description) {
        this.description = description;
    }
    public Set<BiomaterialProperty> getBiomaterialProperties() {
        return this.biomaterialProperties;
    }
    
    public void setBiomaterialProperties(Set<BiomaterialProperty> biomaterialProperties) {
        this.biomaterialProperties = biomaterialProperties;
    }
    public Set<BiomaterialDBXref> getBiomaterialDBXrefs() {
        return this.biomaterialDBXrefs;
    }
    
    public void setBiomaterialDBXrefs(Set<BiomaterialDBXref> biomaterialDBXrefs) {
        this.biomaterialDBXrefs = biomaterialDBXrefs;
    }
    public Set<Treatment> getTreatments() {
        return this.treatments;
    }
    
    public void setTreatments(Set<Treatment> treatments) {
        this.treatments = treatments;
    }
    public Set<BiomaterialRelationship> getParentBiomaterialRelationships() {
        return this.parentBiomaterialRelationships;
    }
    
    public void setParentBiomaterialRelationships(Set<BiomaterialRelationship> parentBiomaterialRelationships) {
        this.parentBiomaterialRelationships = parentBiomaterialRelationships;
    }
    public Set<AssayBiomaterial> getAssayBiomaterials() {
        return this.assayBiomaterials;
    }
    
    public void setAssayBiomaterials(Set<AssayBiomaterial> assayBiomaterials) {
        this.assayBiomaterials = assayBiomaterials;
    }
    public Set<BiomaterialRelationship> getChildBiomaterialRelationships() {
        return this.childBiomaterialRelationships;
    }
    
    public void setChildBiomaterialRelationships(Set<BiomaterialRelationship> childBiomaterialRelationships) {
        this.childBiomaterialRelationships = childBiomaterialRelationships;
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
		 if ( !(other instanceof AbstractBiomaterial) ) return false;
		 AbstractBiomaterial castOther = ( AbstractBiomaterial ) other; 
         
		 return ( (this.getName()==castOther.getName()) || ( this.getName()!=null && castOther.getName()!=null && this.getName().equals(castOther.getName()) ) );
   }
   
   public int hashCode() {
         int result = 17;
         
         
         
         
         
         result = 37 * result + ( getName() == null ? 0 : this.getName().hashCode() );
         
         
         
         
         
         
         
         
         return result;
   }   


}


