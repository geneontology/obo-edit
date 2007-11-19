package org.obo.owl.datamodel.impl;

import java.net.URI;
import java.util.HashSet;
import java.util.Set;

import org.obo.datamodel.CommentedObject;
import org.obo.datamodel.Dbxref;
import org.obo.datamodel.DefinedObject;
import org.obo.datamodel.IdentifiedObject;
import org.obo.owl.dataadapter.OWLAdapter;
import org.obo.owl.datamodel.MetadataMapping;
import org.semanticweb.owl.model.OWLAnnotation;
import org.semanticweb.owl.model.OWLAnnotationAxiom;
import org.semanticweb.owl.model.OWLAxiom;
import org.semanticweb.owl.model.OWLConstant;
import org.semanticweb.owl.model.OWLDataFactory;
import org.semanticweb.owl.model.OWLEntity;
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
				OWLAnnotationAxiom axiom = 
					getAnnotationAxiom(owlEntity,
							getVocabURI(HAS_DEFINITION),
							def);
				axioms.add(axiom);
				for (Dbxref x : ((DefinedObject)io).getDefDbxrefs()) {
					axioms.add(getAxiomAnnotationAxiom(axiom,OWLRDFVocabulary.RDFS_SEE_ALSO.getURI(),
							adapter.getURI(x)));

				}

			}
		}
		return axioms;
	
	}
	
	public boolean translateOWLAxiom(OWLAnnotationAxiom axiom, IdentifiedObject lo) {
		return false;
	}
		
}


		
