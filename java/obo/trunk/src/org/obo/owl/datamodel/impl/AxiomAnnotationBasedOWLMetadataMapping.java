package org.obo.owl.datamodel.impl;

import java.net.URI;
import java.util.HashSet;
import java.util.Set;

import org.obo.datamodel.CommentedObject;
import org.obo.datamodel.Dbxref;
import org.obo.datamodel.DbxrefedObject;
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
import org.semanticweb.owl.model.OWLEntity;
import org.semanticweb.owl.vocab.OWLRDFVocabulary;

public class AxiomAnnotationBasedOWLMetadataMapping extends NCBOOboInOWLMetadataMapping {

	public boolean isOboToOWLLossy() { return true; } // TODO: can be made non-lossy
	
	public String getName() { return "Axiom annotation based mapping"; }
	public String getDesc() { return "The characteristic feature of this mapping is that it uses" +
			" OWL1.1 AxiomAnnotationAxioms instead of the n-ary relation pattern to fully capture synonym and definition" +
			" metadata. Unfortunately, these are currently lost in the RDFXML translation";}
	
	
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
		if (io instanceof SynonymedObject) {
			for (Synonym s : ((SynonymedObject)io).getSynonyms()) {
				OWLAnnotationAxiom axiom = 
					getAnnotationAxiom(owlEntity,
							getVocabURI(HAS_SYNONYM),
							s.getText());
				axioms.add(axiom);
				for (Dbxref x : s.getDbxrefs()) {
					axioms.add(getAxiomAnnotationAxiom(axiom,OWLRDFVocabulary.RDFS_SEE_ALSO.getURI(),
							adapter.getURI(x)));
				}
			}
		}
		if (io instanceof DbxrefedObject) {
			for (Dbxref x : ((DbxrefedObject)io).getDbxrefs()) {
				OWLAnnotationAxiom axiom = 
					getAnnotationAxiom(owlEntity,
							OWLRDFVocabulary.RDFS_SEE_ALSO.getURI(),
							adapter.getURI(x));
			}
		}
		return axioms;
	}
	
	public boolean translateOWLAxiom(OWLAnnotationAxiom axiom, IdentifiedObject lo) {
		return false;
	}
	
		
}


		
