package org.obo.owl.datamodel.impl;

import java.net.URI;
import java.util.HashSet;
import java.util.Set;

import org.obo.datamodel.CommentedObject;
import org.obo.datamodel.DefinedObject;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.ObsoletableObject;
import org.obo.datamodel.SynonymedObject;
import org.obo.datamodel.impl.SynonymImpl;
import org.obo.owl.dataadapter.OWLAdapter;
import org.obo.owl.datamodel.impl.SAO_1_2_OWLMetadataMapping.SAOVocabulary;
import org.obo.owl.util.IDSpaceRegistry;
import org.semanticweb.owl.model.OWLAnnotation;
import org.semanticweb.owl.model.OWLAnnotationAxiom;
import org.semanticweb.owl.model.OWLAxiom;
import org.semanticweb.owl.model.OWLConstant;
import org.semanticweb.owl.model.OWLConstantAnnotation;
import org.semanticweb.owl.model.OWLDataFactory;
import org.semanticweb.owl.model.OWLDataProperty;
import org.semanticweb.owl.model.OWLEntity;
import org.semanticweb.owl.model.OWLIndividual;
import org.semanticweb.owl.model.OWLObjectAnnotation;
import org.semanticweb.owl.vocab.OWLRDFVocabulary;

public class NCBOOboInOWLMetadataMapping extends AbstractOWLMetadataMapping {

	public NCBOOboInOWLMetadataMapping() {
		super();
		IDSpaceRegistry registry = IDSpaceRegistry.getInstance();
		registry.registerMapping("http://www.geneontology.org/formats/oboInOwl#", "OboInOWL");
	}

	public boolean isOboToOWLLossy() { return true; } // TODO: unfinished
	
	public String getName() { return "NCBO OboInOWL mapping"; }
	public String getDesc() { return "See http://www.bioontology.org/wiki/index.php/OboInOwl:Main_Page";}
	
	//public static String ns() { return "http://www.geneontology.org/formats/oboInOwl";}
	public static String ns() { return "http://www.w3.org/2000/01/rdf-schema";}

	public enum OboInOWLNamespaces {

	    OboInOWL("http://www.geneontology.org/formats/oboInOwl#");

	    String ns;
	    OboInOWLNamespaces(String ns) {
	        this.ns = ns;
	    }

	    public String toString() {
	        return ns;
	    }
	}
	
	public enum OboInOWLVocabulary {
		SYNONYM("synonym"),
		HAS_DEFINITION("hasDefinition"),
		OBSOLETE_CLASS("ObsoleteClass");
		
		URI uri;

	    OboInOWLVocabulary(String uri) {
	    	this.uri = URI.create(OboInOWLNamespaces.OboInOWL + uri);
	    }

	    public URI getURI() {
	    	return uri;
	    }
	    
	    public String toString() {
	        return uri.toString();
	    }

	}

	public static final String HAS_DEFINITION = "hasDefinition";
	public static final String HAS_SYNONYM = "hasSynonym";
	
	public URI getVocabURI(String s) {
		return URI.create(ns() + "#" + s);
	}
		
	public Set<OWLAxiom> getOWLAxioms(OWLAdapter adapter, OWLEntity owlEntity, IdentifiedObject io) {
		HashSet<OWLAxiom> axioms = new HashSet<OWLAxiom>();
		setFactory(adapter.getOwlFactory());
		
		OWLDataFactory owlFactory = adapter.getOwlFactory();
		if (io instanceof CommentedObject) {
			String comment = ((CommentedObject)io).getComment();
			if (comment != null && !comment.equals(""))
				axioms.add(getAnnotationAxiom(owlEntity,
						OWLRDFVocabulary.RDFS_COMMENT.getURI(),
						comment));
		}
		if (io instanceof DefinedObject) {
			String def = ((DefinedObject)io).getDefinition();
			if (def != null && !def.equals("")) {
				// n=ary relation
				
				// TODO: is it necessary to create the anon ID?			
				OWLIndividual defInst = 
//					owlFactory.getOWLIndividual(adapter.getURI("_"));
					owlFactory.getOWLIndividual(adapter.getURI(io.getID()+"__def"));
				OWLAnnotationAxiom axiom1 = 
					getAnnotationAxiom(owlEntity,
							getVocabURI(HAS_DEFINITION),
							defInst);
				axioms.add(axiom1);
				
				OWLConstant defCon = owlFactory.getOWLUntypedConstant(def);
				OWLDataProperty rdfsHasLabel = 
					owlFactory.getOWLDataProperty(OWLRDFVocabulary.RDFS_LABEL.getURI());
				OWLAxiom axiom2 = 
					owlFactory.getOWLDataPropertyAssertionAxiom(defInst, 
							rdfsHasLabel, defCon);
				axioms.add(axiom2);

				/*
				OWLProperty hasDbxrefOwlProperty = 
					owlFactory.getOWL getOWLObjectProperty(OWLRDFVocabulary.RDFS_LABEL.getURI())
				for (Dbxref x : ((DefinedObject)io).getDefDbxrefs()) {
					OWLEntity xOwl = owlFactory.getOWLIndividual(adapter.getURI(x));
					axioms.add(owlFactory.getOWLObjectPropertyAssertionAxiom(defInst,
							hasDbxrefOwlProperty, 
							xOwl));
				}
				*/

			}
		}
		if (io instanceof SynonymedObject) {
		}
		return axioms;
	
	}
	
	public boolean translateOWLAxiom(OWLAnnotationAxiom axiom, IdentifiedObject lo, OWLAdapter adapter) {
		OWLAnnotation owlAnnot = axiom.getAnnotation();
		URI uri = owlAnnot.getAnnotationURI();
		if (owlAnnot instanceof OWLConstantAnnotation) {
			String val = owlAnnot.getAnnotationValueAsConstant().getLiteral();
			if (uri.equals(OboInOWLVocabulary.SYNONYM.getURI())) {
				if (lo instanceof SynonymedObject)
					((SynonymedObject)lo).addSynonym(new SynonymImpl(val));
				return true;
			}	
		}
		if (owlAnnot instanceof OWLObjectAnnotation) {
			
			OWLIndividual val = ((OWLObjectAnnotation)owlAnnot).getAnnotationValue();
			String instId = adapter.getOboID(val);
			IdentifiedObject io = adapter.getSession().getObject(instId);
			if (uri.equals(OboInOWLVocabulary.HAS_DEFINITION.getURI())) {
				if (lo instanceof DefinedObject) {
					// relies on instances having been processed first
					if (io != null) {
						((DefinedObject)lo).setDefinition(io.getName());
						adapter.getSession().removeObject(io);
					}
				}
				return true;
			}	
			
		}
		return false;
	}

	public void translateGraph(OBOSession session) {
		for (IdentifiedObject io : session.getObjects()) {
			if (io.isBuiltIn())
				continue;
			if (io instanceof LinkedObject) {
				for (Link link : ((LinkedObject)io).getParents()) {
					if (link.getParent().getID().equals("OboInOWL:ObsoleteClass")) {
						((LinkedObject)io).removeParent(link);
						((ObsoletableObject)io).setObsolete(true);
					}
				}
			}
		}
	}
		
}


		
