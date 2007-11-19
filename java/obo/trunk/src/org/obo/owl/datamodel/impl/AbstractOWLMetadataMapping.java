package org.obo.owl.datamodel.impl;

import java.net.URI;
import java.util.HashSet;
import java.util.Set;

import org.obo.datamodel.CommentedObject;
import org.obo.datamodel.IdentifiedObject;
import org.obo.owl.datamodel.MetadataMapping;
import org.semanticweb.owl.model.OWLAnnotation;
import org.semanticweb.owl.model.OWLAnnotationAxiom;
import org.semanticweb.owl.model.OWLAxiom;
import org.semanticweb.owl.model.OWLAxiomAnnotationAxiom;
import org.semanticweb.owl.model.OWLConstant;
import org.semanticweb.owl.model.OWLDataFactory;
import org.semanticweb.owl.model.OWLEntity;
import org.semanticweb.owl.model.OWLIndividual;
import org.semanticweb.owl.vocab.OWLRDFVocabulary;

public abstract class AbstractOWLMetadataMapping implements MetadataMapping {

	protected OWLDataFactory factory;
	
	public void setFactory(OWLDataFactory factory) {
		this.factory = factory;
	}

	
	protected OWLAnnotation getConstantAnnotation(URI uri, String str) {
		OWLConstant con = factory.getOWLUntypedConstant(str);
		// The above constant is just a plain literal containing the version info text/comment
		// we need to create an annotation, which pairs a URI with the constant
		return
			factory.getOWLConstantAnnotation(uri,con);
	}

	protected OWLAnnotationAxiom getAnnotationAxiom(OWLEntity owlEntity, URI uri, String str) {
		OWLConstant con = factory.getOWLUntypedConstant(str);
		// The above constant is just a plain literal containing the version info text/comment
		// we need to create an annotation, which pairs a URI with the constant
		return
			factory.getOWLEntityAnnotationAxiom(owlEntity, 
				factory.getOWLConstantAnnotation(uri,con));
	}
	protected OWLAnnotationAxiom getAnnotationAxiom(OWLEntity owlEntity, URI pred, URI obj) {
		OWLIndividual i = factory.getOWLIndividual(obj);
		return
			factory.getOWLEntityAnnotationAxiom(owlEntity, 
				factory.getOWLObjectAnnotation(pred,i));
	}
	
	protected OWLAxiomAnnotationAxiom getAxiomAnnotationAxiom(OWLAxiom axiom, URI uri, String str) {
		OWLConstant con = factory.getOWLUntypedConstant(str);
		// The above constant is just a plain literal containing the version info text/comment
		// we need to create an annotation, which pairs a URI with the constant
		return
			factory.getOWLAxiomAnnotationAxiom(axiom, 
					factory.getOWLConstantAnnotation(uri,con));
	}
	
	protected OWLAxiomAnnotationAxiom getAxiomAnnotationAxiom(OWLAxiom axiom, URI pred, URI obj) {
		OWLIndividual i = factory.getOWLIndividual(obj);
		return
			factory.getOWLAxiomAnnotationAxiom(axiom, 
					factory.getOWLObjectAnnotation(pred, i));
	}

}
		
