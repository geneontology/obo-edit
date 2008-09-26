package org.gmod.gbol.simpleObject.generated;


import org.gmod.gbol.simpleObject.*; 

import java.util.Date;
import java.util.HashSet;
import java.util.Set;

/**
 * Acquisition generated by hbm2java
 */
public abstract class AbstractAcquisition extends AbstractSimpleObject implements java.io.Serializable {


     private Integer acquisitionId;
     private Assay assay;
     private Channel channel;
     private Protocol protocol;
     private Date acquisitionDate;
     private String name;
     private String uri;
     private Set<Quantification> quantifications = new HashSet<Quantification>(0);
     private Set<AcquisitionProperty> acquisitionProperties = new HashSet<AcquisitionProperty>(0);
     private Set<AcquisitionRelationship> parentAcquisitionRelationships = new HashSet<AcquisitionRelationship>(0);
     private Set<AcquisitionRelationship> childAcquisitionRelationships = new HashSet<AcquisitionRelationship>(0);

    public AbstractAcquisition() {
    }

	
    public AbstractAcquisition(Assay assay) {
        this.assay = assay;
    }
    public AbstractAcquisition(Assay assay, Channel channel, Protocol protocol, Date acquisitionDate, String name, String uri, Set<Quantification> quantifications, Set<AcquisitionProperty> acquisitionProperties, Set<AcquisitionRelationship> parentAcquisitionRelationships, Set<AcquisitionRelationship> childAcquisitionRelationships) {
       this.assay = assay;
       this.channel = channel;
       this.protocol = protocol;
       this.acquisitionDate = acquisitionDate;
       this.name = name;
       this.uri = uri;
       this.quantifications = quantifications;
       this.acquisitionProperties = acquisitionProperties;
       this.parentAcquisitionRelationships = parentAcquisitionRelationships;
       this.childAcquisitionRelationships = childAcquisitionRelationships;
    }
   
    public Integer getAcquisitionId() {
        return this.acquisitionId;
    }
    
    public void setAcquisitionId(Integer acquisitionId) {
        this.acquisitionId = acquisitionId;
    }
    public Assay getAssay() {
        return this.assay;
    }
    
    public void setAssay(Assay assay) {
        this.assay = assay;
    }
    public Channel getChannel() {
        return this.channel;
    }
    
    public void setChannel(Channel channel) {
        this.channel = channel;
    }
    public Protocol getProtocol() {
        return this.protocol;
    }
    
    public void setProtocol(Protocol protocol) {
        this.protocol = protocol;
    }
    public Date getAcquisitionDate() {
        return this.acquisitionDate;
    }
    
    public void setAcquisitionDate(Date acquisitionDate) {
        this.acquisitionDate = acquisitionDate;
    }
    public String getName() {
        return this.name;
    }
    
    public void setName(String name) {
        this.name = name;
    }
    public String getUri() {
        return this.uri;
    }
    
    public void setUri(String uri) {
        this.uri = uri;
    }
    public Set<Quantification> getQuantifications() {
        return this.quantifications;
    }
    
    public void setQuantifications(Set<Quantification> quantifications) {
        this.quantifications = quantifications;
    }
    public Set<AcquisitionProperty> getAcquisitionProperties() {
        return this.acquisitionProperties;
    }
    
    public void setAcquisitionProperties(Set<AcquisitionProperty> acquisitionProperties) {
        this.acquisitionProperties = acquisitionProperties;
    }
    public Set<AcquisitionRelationship> getParentAcquisitionRelationships() {
        return this.parentAcquisitionRelationships;
    }
    
    public void setParentAcquisitionRelationships(Set<AcquisitionRelationship> parentAcquisitionRelationships) {
        this.parentAcquisitionRelationships = parentAcquisitionRelationships;
    }
    public Set<AcquisitionRelationship> getChildAcquisitionRelationships() {
        return this.childAcquisitionRelationships;
    }
    
    public void setChildAcquisitionRelationships(Set<AcquisitionRelationship> childAcquisitionRelationships) {
        this.childAcquisitionRelationships = childAcquisitionRelationships;
    }


   public boolean equals(Object other) {
         if ( (this == other ) ) return true;
		 if ( (other == null ) ) return false;
		 if ( !(other instanceof AbstractAcquisition) ) return false;
		 AbstractAcquisition castOther = ( AbstractAcquisition ) other; 
         
		 return ( (this.getName()==castOther.getName()) || ( this.getName()!=null && castOther.getName()!=null && this.getName().equals(castOther.getName()) ) );
   }
   
   public int hashCode() {
         int result = 17;
         
         
         
         
         
         
         result = 37 * result + ( getName() == null ? 0 : this.getName().hashCode() );
         
         
         
         
         
         return result;
   }   


}


