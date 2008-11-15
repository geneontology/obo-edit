package org.obo.owl.datamodel.impl;

import java.net.URI;
import java.util.HashSet;
import java.util.Set;

import org.obo.datamodel.CommentedObject;
import org.obo.datamodel.DefinedObject;
import org.obo.datamodel.IdentifiedObject;
import org.obo.owl.dataadapter.OWLAdapter;
import org.semanticweb.owl.model.OWLAnnotation;
import org.semanticweb.owl.model.OWLAnnotationAxiom;
import org.semanticweb.owl.model.OWLAxiom;
import org.semanticweb.owl.model.OWLEntity;
import org.semanticweb.owl.vocab.OWLRDFVocabulary;

import org.apache.log4j.*;

public class SimpleOWLMetadataMapping extends AbstractOWLMetadataMapping {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SimpleOWLMetadataMapping.class);

	public boolean isOboToOWLLossy() { return true; }

	public String getName() { return "Simple mapping"; }
	public String getDesc() { return "Uses basic RDFS vocabulary for all metadata"; }

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
			if (def != null && !def.equals(""))
				axioms.add(getAnnotationAxiom(owlEntity,
						OWLRDFVocabulary.RDFS_COMMENT.getURI(),
						def));
		}
		return axioms;
	}

	public boolean translateOWLAxiom(OWLAnnotationAxiom axiom, IdentifiedObject lo, OWLAdapter adapter) {
		OWLAnnotation owlAnnot = axiom.getAnnotation();
		URI uri = owlAnnot.getAnnotationURI();
		if (owlAnnot.isAnnotationByConstant()) {
			String val = owlAnnot.getAnnotationValueAsConstant().getLiteral();
			if (uri.equals(OWLRDFVocabulary.RDFS_COMMENT.getURI())) {
				if (lo instanceof CommentedObject)
					((CommentedObject)lo).setComment(val);
				return true;
			}		
		}
		return false;
	}

}

