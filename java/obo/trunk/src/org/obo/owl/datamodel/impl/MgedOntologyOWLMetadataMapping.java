package org.obo.owl.datamodel.impl;

import java.net.URI;
import java.util.HashSet;
import java.util.Set;

import org.obo.datamodel.CommentedObject;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.OBOObject;
import org.obo.datamodel.SynonymedObject;
import org.obo.datamodel.impl.SynonymImpl;
import org.obo.owl.dataadapter.OWLAdapter;
import org.obo.owl.util.IDSpaceRegistry;
import org.semanticweb.owl.model.OWLAnnotation;
import org.semanticweb.owl.model.OWLAnnotationAxiom;
import org.semanticweb.owl.model.OWLAxiom;
import org.semanticweb.owl.model.OWLConstantAnnotation;
import org.semanticweb.owl.model.OWLEntity;
import org.semanticweb.owl.vocab.OWLRDFVocabulary;

public class MgedOntologyOWLMetadataMapping extends AbstractOWLMetadataMapping {

	public boolean isOboToOWLLossy() { return true; }
	
	public String getName() { return "MO mapping"; }
	public String getDesc() { return "Uses MO"; }
	
	public String getOWLNamespace()  {
		return "http://mged.sourceforge.net/ontologies/MGEDOntology.owl#";
	}

	public void registerIDSpaces() {
		IDSpaceRegistry.getInstance().registerMapping(getOWLNamespace(),"mged");
		
	}

	public enum MONamespaces {

	    MO("http://mged.sourceforge.net/ontologies/MGEDOntology.owl#");

	    String ns;
	    MONamespaces(String ns) {
	        this.ns = ns;
	    }

	    public String toString() {
	        return ns;
	    }
	}
	
	public enum MOVocabulary {
		DEPRECATION_IN("deprecation_in"),
		DEPRECATED_FROM_VERSION("deprecated_from_version"),
		DEPRECATION_REPLACEMENT_TERM("deprecation_replacement_term"),
		WAS_REPLACED_BY("was_replaced_by"),
		SYNONYM("synonym");
		URI uri;

	    MOVocabulary(String uri) {
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
		if (!(lo instanceof OBOObject))
			return false;
		OBOObject obj = (OBOObject) lo;
		if (owlAnnot instanceof OWLConstantAnnotation) {
			String val = owlAnnot.getAnnotationValueAsConstant().getLiteral();

			if (uri.equals(OWLRDFVocabulary.RDFS_COMMENT.getURI())) {
				if (lo instanceof CommentedObject)
					((CommentedObject)lo).setComment(val);
				return true;
			}		

			if (uri.equals(MOVocabulary.SYNONYM.getURI())) {
				obj.addSynonym(new SynonymImpl(val));
				return true;
			}	
			
			if (uri.equals(MOVocabulary.DEPRECATION_IN.getURI())) {
				obj.setObsolete(true);
				return true;
			}	
			if (uri.equals(MOVocabulary.DEPRECATION_REPLACEMENT_TERM.getURI()) ||
					uri.equals(MOVocabulary.WAS_REPLACED_BY.getURI())) {
				obj.setObsolete(true);
//				obj.addReplacedBy(o); TODO
				return true;
			}	
		}
		return false;
	}

	


}
		
