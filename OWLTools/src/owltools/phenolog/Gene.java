package owltools.phenolog;

//import org.semanticweb.owlapi.model.OWLObject;

/**
 * Represents a characteristic of an individual.
 * This could be represented using an ID for an ontology class.
 *
 */
public class Gene implements Comparable<Gene>{
    String id;
    String label;

    @Override
    public boolean equals(Object aGene){
        Gene g = (Gene) aGene;
        return getid().equals(g.getid());
    }
    @Override
    public int hashCode(){
        return id.hashCode();
    }
    @Override
    public int compareTo(Gene g){
        return id.compareTo(g.getid());
    }


    public Gene(String id, String label){
        this.id = id;
        this.label = label;
    }

    public Gene(String id){
        this.id = id;
    }

    public void setid(String id){
        this.id = id;
    }
    public void setlabel(String label){
        this.label = label;
    }

    public String getid(){
        return this.id;
    }
    public String getlabel(){
        return this.label;
    }
}