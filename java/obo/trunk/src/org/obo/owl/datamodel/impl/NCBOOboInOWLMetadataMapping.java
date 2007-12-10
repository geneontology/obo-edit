package org.obo.owl.datamodel.impl;

import java.net.URI;
import java.util.HashSet;
import java.util.Set;

import org.obo.datamodel.CommentedObject;
import org.obo.datamodel.Dbxref;
import org.obo.datamodel.DefinedObject;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.SynonymedObject;
import org.obo.owl.dataadapter.OWLAdapter;
import org.obo.owl.datamodel.MetadataMapping;
import org.semanticweb.owl.model.OWLAnnotation;
import org.semanticweb.owl.model.OWLAnnotationAxiom;
import org.semanticweb.owl.model.OWLAxiom;
import org.semanticweb.owl.model.OWLConstant;
import org.semanticweb.owl.model.OWLDataFactory;
import org.semanticweb.owl.model.OWLDataProperty;
import org.semanticweb.owl.model.OWLEntity;
import org.semanticweb.owl.model.OWLIndividual;
import org.semanticweb.owl.vocab.Namespaces;
import org.semanticweb.owl.vocab.OWLRDFVocabulary;

public class NCBOOboInOWLMetadataMapping extends AbstractOWLMetadataMapping {

	public boolean isOboToOWLLossy() { return true; } // TODO: unfinished
	
	public String getName() { return "NCBO OboInOWL mapping"; }
	public String getDesc() { return "See http://www.bioontology.org/wiki/index.php/OboInOwl:Main_Page";}
	
	//public static String ns() { return "http://www.geneontology.org/formats/oboInOwl";}
	public static String ns() { return "http://www.w3.org/2000/01/rdf-schema";}

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
	
	public boolean translateOWLAxiom(OWLAnnotationAxiom axiom, IdentifiedObject lo) {
		return false;
	}
		
}


		
