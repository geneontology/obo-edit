package org.obo.owl.datamodel.impl;

import java.net.URI;
import java.util.HashSet;
import java.util.Set;

import org.obo.datamodel.CommentedObject;
import org.obo.datamodel.Dbxref;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.OBOObject;
import org.obo.datamodel.impl.DbxrefImpl;
import org.obo.datamodel.impl.SynonymImpl;
import org.obo.owl.dataadapter.OWLAdapter;
import org.obo.owl.datamodel.impl.NCBOOboInOWLMetadataMapping.OboInOWLNamespaces;
import org.obo.owl.util.IDSpaceRegistry;
import org.semanticweb.owl.model.OWLAnnotation;
import org.semanticweb.owl.model.OWLAnnotationAxiom;
import org.semanticweb.owl.model.OWLAxiom;
import org.semanticweb.owl.model.OWLConstantAnnotation;
import org.semanticweb.owl.model.OWLEntity;
import org.semanticweb.owl.vocab.OWLRDFVocabulary;

import org.apache.log4j.*;

public class OBIMetadataMapping extends AbstractOWLMetadataMapping {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OBIMetadataMapping.class);

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
		DEFINITION_EDITOR("definition_editor"),
		DEFINITION_SOURCE("definition_source"),
		SYNONYM("alternative_term");
		URI uri;

	    OBIVocabulary(String uri) {
	    	this.uri = URI.create(OBINamespaces.OBI + uri);
	    }

	    public URI getURI() {
	    	return uri;
	    }
	    
	    public String toString() {
	        return uri.toString();
	    }
	}
	
	public void registerIDSpaces() {
		IDSpaceRegistry.getInstance().registerMapping("http://obi.sourceforge.net/ontology/OBI.owl#","obi");	
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
			if (uri.equals(OBIVocabulary.DEFINITION_EDITOR.getURI()) ||
					uri.equals(OBIVocabulary.DEFINITION_SOURCE.getURI())) {
				String db = "obi:";
				DbxrefImpl xref = new DbxrefImpl(db,val);
				xref.setType(Dbxref.DEFINITION);
				if (lo instanceof OBOObject)
					((OBOObject)lo).addDefDbxref(xref);
				return true;
			}	
			if (uri.equals(OWLRDFVocabulary.RDFS_COMMENT.getURI())) {
				if (lo instanceof CommentedObject)
					((CommentedObject)lo).setComment(val);
				return true;
			}		
		}
		return false;
	}

}
		
