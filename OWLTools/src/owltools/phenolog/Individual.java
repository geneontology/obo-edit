package owltools.phenolog;

import java.util.HashSet;
import java.util.Set;

//import org.semanticweb.owlapi.model.OWLObject;

/**
 * represents an attribute-bearing entity; for example, a gene.
 */
public class Individual {

	private String id;
	private String label;
	//private OWLObject owlObject; // for future use
	private Set<Attribute> attributes = new HashSet<Attribute>();

        public Individual(String id, String label, Set<Attribute> attributes){
            this.id = id;
            this.label = label;
            this.attributes = attributes;
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
    /*public OWLObject getOwlObject() {
    return owlObject;
    }
    public void setOwlObject(OWLObject owlObject) {
    this.owlObject = owlObject;
    }*/


	public Set<Attribute> getAttributes() {
		return attributes;
	}

	public void setAttributes(Set<Attribute> attributes) {
		this.attributes = attributes;
	}
}
