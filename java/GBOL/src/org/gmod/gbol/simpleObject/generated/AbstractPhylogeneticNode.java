package org.gmod.gbol.simpleObject.generated;


import org.gmod.gbol.simpleObject.*; 

import java.util.HashSet;
import java.util.Set;

/**
 * PhylogeneticNode generated by hbm2java
 */
public abstract class AbstractPhylogeneticNode  implements java.io.Serializable {


     private Integer phylogeneticNodeId;
     private PhylogeneticNode parentPhylogeneticNode;
     private CVTerm type;
     private Feature feature;
     private PhylogeneticTree phylogeneticTree;
     private int leftIndex;
     private int rightIndex;
     private String label;
     private Double distance;
     private Set<PhylogeneticNodeRelationship> childPhylogeneticNodeRelationships = new HashSet<PhylogeneticNodeRelationship>(0);
     private Set<PhylogeneticNodeOrganism> phylogeneticNodeOrganisms = new HashSet<PhylogeneticNodeOrganism>(0);
     private Set<PhylogeneticNodePublication> phylogeneticNodePublications = new HashSet<PhylogeneticNodePublication>(0);
     private Set<PhylogeneticNodeRelationship> parentPhylogeneticNodeRelationships = new HashSet<PhylogeneticNodeRelationship>(0);
     private Set<PhylogeneticNodeDBXref> phylogeneticNodeDBXrefs = new HashSet<PhylogeneticNodeDBXref>(0);
     private Set<PhylogeneticNodeProperty> phylogeneticNodeProperties = new HashSet<PhylogeneticNodeProperty>(0);

    public AbstractPhylogeneticNode() {
    }

	
    public AbstractPhylogeneticNode(PhylogeneticTree phylogeneticTree, int leftIndex, int rightIndex) {
        this.phylogeneticTree = phylogeneticTree;
        this.leftIndex = leftIndex;
        this.rightIndex = rightIndex;
    }
    public AbstractPhylogeneticNode(PhylogeneticNode parentPhylogeneticNode, CVTerm type, Feature feature, PhylogeneticTree phylogeneticTree, int leftIndex, int rightIndex, String label, Double distance, Set<PhylogeneticNodeRelationship> childPhylogeneticNodeRelationships, Set<PhylogeneticNodeOrganism> phylogeneticNodeOrganisms, Set<PhylogeneticNodePublication> phylogeneticNodePublications, Set<PhylogeneticNodeRelationship> parentPhylogeneticNodeRelationships, Set<PhylogeneticNodeDBXref> phylogeneticNodeDBXrefs, Set<PhylogeneticNodeProperty> phylogeneticNodeProperties) {
       this.parentPhylogeneticNode = parentPhylogeneticNode;
       this.type = type;
       this.feature = feature;
       this.phylogeneticTree = phylogeneticTree;
       this.leftIndex = leftIndex;
       this.rightIndex = rightIndex;
       this.label = label;
       this.distance = distance;
       this.childPhylogeneticNodeRelationships = childPhylogeneticNodeRelationships;
       this.phylogeneticNodeOrganisms = phylogeneticNodeOrganisms;
       this.phylogeneticNodePublications = phylogeneticNodePublications;
       this.parentPhylogeneticNodeRelationships = parentPhylogeneticNodeRelationships;
       this.phylogeneticNodeDBXrefs = phylogeneticNodeDBXrefs;
       this.phylogeneticNodeProperties = phylogeneticNodeProperties;
    }
   
    public Integer getPhylogeneticNodeId() {
        return this.phylogeneticNodeId;
    }
    
    public void setPhylogeneticNodeId(Integer phylogeneticNodeId) {
        this.phylogeneticNodeId = phylogeneticNodeId;
    }
    public PhylogeneticNode getParentPhylogeneticNode() {
        return this.parentPhylogeneticNode;
    }
    
    public void setParentPhylogeneticNode(PhylogeneticNode parentPhylogeneticNode) {
        this.parentPhylogeneticNode = parentPhylogeneticNode;
    }
    public CVTerm getType() {
        return this.type;
    }
    
    public void setType(CVTerm type) {
        this.type = type;
    }
    public Feature getFeature() {
        return this.feature;
    }
    
    public void setFeature(Feature feature) {
        this.feature = feature;
    }
    public PhylogeneticTree getPhylogeneticTree() {
        return this.phylogeneticTree;
    }
    
    public void setPhylogeneticTree(PhylogeneticTree phylogeneticTree) {
        this.phylogeneticTree = phylogeneticTree;
    }
    public int getLeftIndex() {
        return this.leftIndex;
    }
    
    public void setLeftIndex(int leftIndex) {
        this.leftIndex = leftIndex;
    }
    public int getRightIndex() {
        return this.rightIndex;
    }
    
    public void setRightIndex(int rightIndex) {
        this.rightIndex = rightIndex;
    }
    public String getLabel() {
        return this.label;
    }
    
    public void setLabel(String label) {
        this.label = label;
    }
    public Double getDistance() {
        return this.distance;
    }
    
    public void setDistance(Double distance) {
        this.distance = distance;
    }
    public Set<PhylogeneticNodeRelationship> getChildPhylogeneticNodeRelationships() {
        return this.childPhylogeneticNodeRelationships;
    }
    
    public void setChildPhylogeneticNodeRelationships(Set<PhylogeneticNodeRelationship> childPhylogeneticNodeRelationships) {
        this.childPhylogeneticNodeRelationships = childPhylogeneticNodeRelationships;
    }
    public Set<PhylogeneticNodeOrganism> getPhylogeneticNodeOrganisms() {
        return this.phylogeneticNodeOrganisms;
    }
    
    public void setPhylogeneticNodeOrganisms(Set<PhylogeneticNodeOrganism> phylogeneticNodeOrganisms) {
        this.phylogeneticNodeOrganisms = phylogeneticNodeOrganisms;
    }
    public Set<PhylogeneticNodePublication> getPhylogeneticNodePublications() {
        return this.phylogeneticNodePublications;
    }
    
    public void setPhylogeneticNodePublications(Set<PhylogeneticNodePublication> phylogeneticNodePublications) {
        this.phylogeneticNodePublications = phylogeneticNodePublications;
    }
    public Set<PhylogeneticNodeRelationship> getParentPhylogeneticNodeRelationships() {
        return this.parentPhylogeneticNodeRelationships;
    }
    
    public void setParentPhylogeneticNodeRelationships(Set<PhylogeneticNodeRelationship> parentPhylogeneticNodeRelationships) {
        this.parentPhylogeneticNodeRelationships = parentPhylogeneticNodeRelationships;
    }
    public Set<PhylogeneticNodeDBXref> getPhylogeneticNodeDBXrefs() {
        return this.phylogeneticNodeDBXrefs;
    }
    
    public void setPhylogeneticNodeDBXrefs(Set<PhylogeneticNodeDBXref> phylogeneticNodeDBXrefs) {
        this.phylogeneticNodeDBXrefs = phylogeneticNodeDBXrefs;
    }
    public Set<PhylogeneticNodeProperty> getPhylogeneticNodeProperties() {
        return this.phylogeneticNodeProperties;
    }
    
    public void setPhylogeneticNodeProperties(Set<PhylogeneticNodeProperty> phylogeneticNodeProperties) {
        this.phylogeneticNodeProperties = phylogeneticNodeProperties;
    }


   public boolean equals(Object other) {
         if ( (this == other ) ) return true;
		 if ( (other == null ) ) return false;
		 if ( !(other instanceof AbstractPhylogeneticNode) ) return false;
		 AbstractPhylogeneticNode castOther = ( AbstractPhylogeneticNode ) other; 
         
		 return ( (this.getLabel()==castOther.getLabel()) || ( this.getLabel()!=null && castOther.getLabel()!=null && this.getLabel().equals(castOther.getLabel()) ) );
   }
   
   public int hashCode() {
         int result = 17;
         
         
         
         
         
         
         
         
         result = 37 * result + ( getLabel() == null ? 0 : this.getLabel().hashCode() );
         
         
         
         
         
         
         
         return result;
   }   


}


