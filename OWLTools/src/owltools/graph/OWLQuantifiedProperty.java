package owltools.graph;

import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLObjectInverseOf;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLProperty;
import org.semanticweb.owlapi.model.OWLPropertyExpression;

public class OWLQuantifiedProperty {
	
	// TODO - change named to predicate?
	public enum Quantifier {
		SOME, ONLY, CARDINALITY, SUBCLASS_OF, INSTANCE_OF
	}
	
	private OWLObjectProperty property;
	private Quantifier quantifier;
	private boolean isInverseOf = false;
	private boolean isNegated = false;
	private Integer minCardinality;
	private Integer maxCardinality;

	public OWLQuantifiedProperty(OWLObjectPropertyExpression p, Quantifier q) {
		if (p instanceof OWLObjectInverseOf) {
			isInverseOf = true;
			p = ((OWLObjectInverseOf)p).getInverse();
		}
		property = p.asOWLObjectProperty();
		this.quantifier = q;
	}
	
	public OWLQuantifiedProperty() {
		// TODO Auto-generated constructor stub
	}

	public OWLQuantifiedProperty(Quantifier q) {
		this.quantifier = q;
	}

	public boolean isInverseOf() {
		return isInverseOf;
	}

	public void setInverseOf(boolean isInverseOf) {
		this.isInverseOf = isInverseOf;
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
	
	public String toString() {
		return getPropertyId()+" "+quantifier;
	}
	
	private String getPropertyId() {
		if (property == null)
			return "subclass";
		return property.getIRI().toString();
	}

	// TODO - overload quantifier?
	public boolean isSubClassOf() {
		return property == null || quantifier == Quantifier.SUBCLASS_OF;
	}
	public boolean isInstanceOf() {
		return quantifier != null && quantifier == Quantifier.INSTANCE_OF;
	}

	public boolean isSomeValuesFrom() {
		return quantifier != null && quantifier == Quantifier.SOME;
	}

	public boolean isAllValuesFrom() {
		return quantifier != null && quantifier == Quantifier.ONLY;
	}
	
	public boolean isCardinality() {
		return quantifier != null && quantifier == Quantifier.CARDINALITY;
	}


}
