package org.obo.owl.datamodel.impl;

import java.net.URI;
import java.util.HashSet;
import java.util.Set;

import org.obo.datamodel.CommentedObject;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.OBOObject;
import org.obo.datamodel.impl.SynonymImpl;
import org.obo.owl.dataadapter.OWLAdapter;
import org.semanticweb.owl.model.OWLAnnotation;
import org.semanticweb.owl.model.OWLAnnotationAxiom;
import org.semanticweb.owl.model.OWLAxiom;
import org.semanticweb.owl.model.OWLConstantAnnotation;
import org.semanticweb.owl.model.OWLEntity;
import org.semanticweb.owl.vocab.OWLRDFVocabulary;

public class OBIMetadataMapping extends AbstractOWLMetadataMapping {

	public boolean isOboToOWLLossy() { return true; }
	
	public String getName() { return "OBI mapping"; }
	public String getDesc() { return "Uses OBI annotation properties"; }
	
	public String ns = "http://obi.sourceforge.net/ontology/OBI.owl#";
	
	public enum OBINamespaces {

	    OBI("http://obi.sourceforge.net/ontology/OBI.owl#");

	    String ns;
	    OBINamespaces(String ns) {
	        this.ns = ns;
	    }

	    public String toString() {
	        return ns;
	    }
	}
	
	public enum OBIVocabulary {
		DEFINITION("definition"),
		SYNONYM("alternative_term");
		URI uri;

	    OBIVocabulary(String uri) {
	    	this.uri = URI.create(uri);
	    }

	    public URI getURI() {
	    	return uri;
	    }
	}
	
	public Set<OWLAxiom> getOWLAxioms(OWLAdapter adapter, OWLEntity owlEntity, IdentifiedObject io) {
		HashSet<OWLAxiom> axioms = new HashSet<OWLAxiom>();
		setFactory(adapter.getOwlFactory());
		if (io instanceof CommentedObject) {
			 String comment = ((CommentedObject)io).getComment();
			 if (comment != null && !comment.equals(""))
				 axioms.add(getAnnotationAxiom(owlEntity,
								 OWLRDFVocabulary.RDFS_COMMENT.getURI(),
								 comment));
		}
		return axioms;
	}

	public boolean translateOWLAxiom(OWLAnnotationAxiom axiom, IdentifiedObject lo, OWLAdapter adapter) {
		OWLAnnotation owlAnnot = axiom.getAnnotation();
		URI uri = owlAnnot.getAnnotationURI();
		if (owlAnnot instanceof OWLConstantAnnotation) {
			String val = owlAnnot.getAnnotationValueAsConstant().getLiteral();
			if (uri.equals(OBIVocabulary.SYNONYM.getURI())) {
				if (lo instanceof OBOObject) // TODO - use OBOObject everywhere
					((OBOObject)lo).addSynonym(new SynonymImpl(val));
				return true;
			}	
			if (uri.equals(OBIVocabulary.DEFINITION.getURI())) {
				if (lo instanceof OBOObject)
					((OBOObject)lo).setDefinition(val);
				return true;
			}	
		}
		return false;
	}

}
		
