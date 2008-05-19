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
import org.semanticweb.owl.model.OWLAnnotation;
import org.semanticweb.owl.model.OWLAnnotationAxiom;
import org.semanticweb.owl.model.OWLAxiom;
import org.semanticweb.owl.model.OWLConstantAnnotation;
import org.semanticweb.owl.model.OWLEntity;
import org.semanticweb.owl.vocab.OWLRDFVocabulary;

import org.apache.log4j.*;

public class SAO_1_2_OWLMetadataMapping extends AbstractOWLMetadataMapping {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SAO_1_2_OWLMetadataMapping.class);

	public boolean isOboToOWLLossy() { return true; }
	
	public String getName() { return "SAO 1.2 mapping"; }
	public String getDesc() { return "Uses SAO"; }
	
	public String ns = "http://ccdb.ucsd.edu/SAO/1.2#";
	
	public enum SAONamespaces {

	    SAO("http://ccdb.ucsd.edu/SAO/1.2#");

	    String ns;
	    SAONamespaces(String ns) {
	        this.ns = ns;
	    }

	    public String toString() {
	        return ns;
	    }
	}
	
	public enum SAOVocabulary {
		CL_ID("cell_ontology_ID"),
		GO_ID("gene_Ontology_ID"),
		UMLS_ID("umls_ID"),
		BONFIRE_ID("bonfire_ID"),
		SYNONYM("synonym");
		URI uri;

	    SAOVocabulary(String uri) {
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
			if (uri.equals(SAOVocabulary.SYNONYM.getURI())) {
				obj.addSynonym(new SynonymImpl(val));
				return true;
			}	
			else if (uri.equals(SAOVocabulary.CL_ID.getURI())) {
				addDbxref(obj, "CL", val);
				return true;
			}	
			else if (uri.equals(SAOVocabulary.GO_ID.getURI())) {
				addDbxref(obj, "GO", val);
				return true;
			}	
			else if (uri.equals(SAOVocabulary.UMLS_ID.getURI())) {
				addDbxref(obj, "UMLS", val);
				return true;
			}	
			else if (uri.equals(SAOVocabulary.BONFIRE_ID.getURI())) {
				addDbxref(obj, "BONFIRE", val);
				return true;
			}	
		}
		return false;
	}



}
		
