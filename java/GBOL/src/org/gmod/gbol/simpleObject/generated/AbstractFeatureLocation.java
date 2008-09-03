package org.gmod.gbol.simpleObject.generated;


import org.gmod.gbol.simpleObject.*; 

import java.util.HashSet;
import java.util.Set;

/**
 * FeatureLocation generated by hbm2java
 */
public abstract class AbstractFeatureLocation  implements java.io.Serializable {


     private Integer featureLocationId;
     private Feature sourceFeature;
     private Feature feature;
     private Integer fmin;
     private boolean isFminPartial;
     private Integer fmax;
     private boolean isFmaxPartial;
     private Short strand;
     private Integer phase;
     private String residueInfo;
     private int locgroup;
     private int rank;
     private Set<FeatureLocationPublication> featureLocationPublications = new HashSet<FeatureLocationPublication>(0);

    public AbstractFeatureLocation() {
    }

	
    public AbstractFeatureLocation(Feature feature, boolean isFminPartial, boolean isFmaxPartial, int locgroup, int rank) {
        this.feature = feature;
        this.isFminPartial = isFminPartial;
        this.isFmaxPartial = isFmaxPartial;
        this.locgroup = locgroup;
        this.rank = rank;
    }
    public AbstractFeatureLocation(Feature sourceFeature, Feature feature, Integer fmin, boolean isFminPartial, Integer fmax, boolean isFmaxPartial, Short strand, Integer phase, String residueInfo, int locgroup, int rank, Set<FeatureLocationPublication> featureLocationPublications) {
       this.sourceFeature = sourceFeature;
       this.feature = feature;
       this.fmin = fmin;
       this.isFminPartial = isFminPartial;
       this.fmax = fmax;
       this.isFmaxPartial = isFmaxPartial;
       this.strand = strand;
       this.phase = phase;
       this.residueInfo = residueInfo;
       this.locgroup = locgroup;
       this.rank = rank;
       this.featureLocationPublications = featureLocationPublications;
    }
   
    public Integer getFeatureLocationId() {
        return this.featureLocationId;
    }
    
    public void setFeatureLocationId(Integer featureLocationId) {
        this.featureLocationId = featureLocationId;
    }
    public Feature getSourceFeature() {
        return this.sourceFeature;
    }
    
    public void setSourceFeature(Feature sourceFeature) {
        this.sourceFeature = sourceFeature;
    }
    public Feature getFeature() {
        return this.feature;
    }
    
    public void setFeature(Feature feature) {
        this.feature = feature;
    }
    public Integer getFmin() {
        return this.fmin;
    }
    
    public void setFmin(Integer fmin) {
        this.fmin = fmin;
    }
    public boolean isIsFminPartial() {
        return this.isFminPartial;
    }
    
    public void setIsFminPartial(boolean isFminPartial) {
        this.isFminPartial = isFminPartial;
    }
    public Integer getFmax() {
        return this.fmax;
    }
    
    public void setFmax(Integer fmax) {
        this.fmax = fmax;
    }
    public boolean isIsFmaxPartial() {
        return this.isFmaxPartial;
    }
    
    public void setIsFmaxPartial(boolean isFmaxPartial) {
        this.isFmaxPartial = isFmaxPartial;
    }
    public Short getStrand() {
        return this.strand;
    }
    
    public void setStrand(Short strand) {
        this.strand = strand;
    }
    public Integer getPhase() {
        return this.phase;
    }
    
    public void setPhase(Integer phase) {
        this.phase = phase;
    }
    public String getResidueInfo() {
        return this.residueInfo;
    }
    
    public void setResidueInfo(String residueInfo) {
        this.residueInfo = residueInfo;
    }
    public int getLocgroup() {
        return this.locgroup;
    }
    
    public void setLocgroup(int locgroup) {
        this.locgroup = locgroup;
    }
    public int getRank() {
        return this.rank;
    }
    
    public void setRank(int rank) {
        this.rank = rank;
    }
    public Set<FeatureLocationPublication> getFeatureLocationPublications() {
        return this.featureLocationPublications;
    }
    
    public void setFeatureLocationPublications(Set<FeatureLocationPublication> featureLocationPublications) {
        this.featureLocationPublications = featureLocationPublications;
    }


   public boolean equals(Object other) {
         if ( (this == other ) ) return true;
		 if ( (other == null ) ) return false;
		 if ( !(other instanceof AbstractFeatureLocation) ) return false;
		 AbstractFeatureLocation castOther = ( AbstractFeatureLocation ) other; 
         
		 return ( (this.getFeature()==castOther.getFeature()) || ( this.getFeature()!=null && castOther.getFeature()!=null && this.getFeature().equals(castOther.getFeature()) ) )
 && (this.getLocgroup()==castOther.getLocgroup())
 && (this.getRank()==castOther.getRank());
   }
   
   public int hashCode() {
         int result = 17;
         
         
         
         result = 37 * result + ( getFeature() == null ? 0 : this.getFeature().hashCode() );
         
         
         
         
         
         
         
         result = 37 * result + this.getLocgroup();
         result = 37 * result + this.getRank();
         
         return result;
   }   


}


