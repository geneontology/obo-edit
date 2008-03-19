package org.obo.owl.datamodel.impl;

import java.net.URI;

import org.obo.datamodel.Dbxref;
import org.obo.datamodel.OBOObject;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.impl.DbxrefImpl;
import org.obo.owl.datamodel.MetadataMapping;
import org.semanticweb.owl.model.OWLAnnotation;
import org.semanticweb.owl.model.OWLAnnotationAxiom;
import org.semanticweb.owl.model.OWLAxiom;
import org.semanticweb.owl.model.OWLAxiomAnnotationAxiom;
import org.semanticweb.owl.model.OWLConstant;
import org.semanticweb.owl.model.OWLDataFactory;
import org.semanticweb.owl.model.OWLEntity;
import org.semanticweb.owl.model.OWLIndividual;

public abstract class AbstractOWLMetadataMapping implements MetadataMapping {

	protected OWLDataFactory factory;
	protected OBOSession session;
	
	
	
	public AbstractOWLMetadataMapping() {
		super();
		registerIDSpaces();
	}
	
	public void registerIDSpaces() {
		
	}

	public OBOSession getSession() {
		return session;
	}

	public void setSession(OBOSession session) {
		this.session = session;
	}


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
	
	protected OWLAnnotationAxiom getAnnotationAxiom(OWLEntity owlEntity, URI uri, OWLIndividual x) {
		// The above constant is just a plain literal containing the version info text/comment
		// we need to create an annotation, which pairs a URI with the constant
		return
			factory.getOWLEntityAnnotationAxiom(owlEntity,
				factory.getOWLObjectAnnotation(uri, x));
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
		OWLIndividual i = factory.getOWLIndividual(obj); // always an individual?s
		return
			factory.getOWLAxiomAnnotationAxiom(axiom, 
					factory.getOWLObjectAnnotation(pred, i));
	}

	public void translateGraph(OBOSession session) {
		// TODO Auto-generated method stub
	}
	
	protected void addDbxref(OBOObject obj, String db, String localID) {
		Dbxref xref = new DbxrefImpl(db,localID);
		obj.addDbxref(xref);
	}

}
		
