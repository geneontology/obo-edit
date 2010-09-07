package owltools.phenolog;

//import org.semanticweb.owlapi.model.OWLObject;

/**
 * Represents a characteristic of an individual.
 * This could be represented using an ID for an ontology class.
 *
 */
public class GenePheno implements Comparable<GenePheno>{
    String id;
    String phenoid;
    String phenolabel;

    @Override
    public boolean equals(Object aGenePheno){
        GenePheno gp = (GenePheno) aGenePheno;
        return getid().equals(gp.getid());
    }
    @Override
    public int hashCode(){
        return id.hashCode();
    }
    @Override
    public int compareTo(GenePheno g){
        return id.compareTo(g.getid());
    }

    
    public GenePheno(String id, String phenoid, String phenolabel){
        this.id = id;
        this.phenoid = phenoid;
        this.phenolabel = phenolabel;
    }

    public GenePheno(String id, String phenoid){
        this.id = id;
        this.phenoid = phenoid;
    }

    public void setid(String id){
        this.id = id;
    }
    public void setphenoid(String phenoid){
        this.phenoid = phenoid;
    }
    public void setphenolabel(String phenolabel){
        this.phenolabel = phenolabel;
    }

    public String getid(){
        return this.id;
    }
    public String getphenoid(){
        return this.phenoid;
    }
    public String getphenolabel(){
        return this.phenolabel;
    }
}
