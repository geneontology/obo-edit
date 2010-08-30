package owltools.graph;

import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLObjectInverseOf;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLProperty;
import org.semanticweb.owlapi.model.OWLPropertyExpression;

public class OWLGraphEdgeLabel {
	
	 public enum Quantifier {
		SOME, ONLY, NOT, CARDINALITY
	}
	
	private OWLObjectProperty property;
	private Quantifier quantifier;
	private boolean isInverseOf = false;
	
	private Integer minCardinality;
	private Integer maxCardinality;

	public OWLGraphEdgeLabel(OWLObjectPropertyExpression p, Quantifier q) {
		if (p instanceof OWLObjectInverseOf) {
			isInverseOf = true;
			p = ((OWLObjectInverseOf)p).getInverse();
		}
		property = p.asOWLObjectProperty();
		this.quantifier = q;
	}
	public OWLObjectProperty getProperty() {
		return property;
	}
	public void setProperty(OWLObjectProperty property) {
		this.property = property;
	}
	public Quantifier getQuantifier() {
		return quantifier;
	}
	public void setQuantifier(Quantifier quantifier) {
		this.quantifier = quantifier;
	}
	public Integer getMinCardinality() {
		return minCardinality;
	}
	public void setMinCardinality(Integer minCardinality) {
		this.minCardinality = minCardinality;
	}
	public Integer getMaxCardinality() {
		return maxCardinality;
	}
	public void setMaxCardinality(Integer maxCardinality) {
		this.maxCardinality = maxCardinality;
	}
	
	

}
