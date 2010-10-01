package owltools.phenolog;


import java.util.HashSet;
import java.util.Set;

//import org.semanticweb.owlapi.model.OWLObject;

/**
 * represents an attribute-bearing entity; for example, a phenotype.
 */


public class Pheno implements Comparable<Pheno>{

	private String id;
	private String label;
        private Pheno closest;
        private double closestdistance;
        private int closestoverlap;
        private HashSet<IndividualPair> closestoverlappairs;
	//private OWLObject owlObject; // for future use
	private Set<Individual> individuals = new HashSet<Individual>();


//        @Override
        public boolean equals(Object aPheno){
            Pheno p = (Pheno) aPheno;
            return getId().equals(p.getId());
        }
//        @Override
        public int hashCode(){
            return id.hashCode();
        }
//        @Override
        public int compareTo(Pheno p){
            return id.compareTo(p.getId());
        }

        public Pheno(){

        }
        public Pheno(String id){
            this.id = id;
        }
        public Pheno(String id, String label, Set<Individual> individuals){
            this.id = id;
            this.label = label;
            this.individuals = individuals;
        }

	public String getId() {
            return id;
	}
	public void setId(String id) {
            this.id = id;
	}

	public String getLabel() {
            return label;
	}
	public void setLabel(String label) {
            this.label = label;
	}

        public void setClosest(Pheno closest){
            this.closest = closest;
        }
        public Pheno getClosest(){
            return closest;
        }

        public void setClosestDistance(double closestdistance){
            this.closestdistance = closestdistance;
        }
        public double getClosestDistance(){
            return closestdistance;
        }

        public void setClosestOverlap(int closestoverlap){
            this.closestoverlap = closestoverlap;
        }
        public int getClosestOverlap(){
            return closestoverlap;
        }

        public void setClosestOverlapPairs(HashSet<IndividualPair> hsip){
            this.closestoverlappairs = hsip;
        }
        public HashSet<IndividualPair> getClosestOverlapPairs(){
            return closestoverlappairs;
        }
    /*public OWLObject getOwlObject() {
    return owlObject;
    }
    public void setOwlObject(OWLObject owlObject) {
    this.owlObject = owlObject;
    }*/


	public Set<Individual> getIndividuals() {
		return individuals;
	}

	public void setIndividuals(Set<Individual> individuals) {
		this.individuals = individuals;
	}
}