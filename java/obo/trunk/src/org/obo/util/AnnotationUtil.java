package org.obo.util;

import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;

import org.obo.annotation.datamodel.Annotation;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOObject;
import org.obo.datamodel.OBOSession;

import org.apache.log4j.*;

public class AnnotationUtil {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AnnotationUtil.class);

	public static Collection<Annotation> getAnnotationsForSubject(OBOSession session, IdentifiedObject su) {
		Collection<Annotation> annots = new LinkedList<Annotation>();
		for (IdentifiedObject io : session.getObjects()) {
			if (io instanceof Annotation) {
				Annotation annot = (Annotation)io;
				// Extra defence against null annotations from Phenote
				if (annot.getSubject() != null &&
						annot.getObject() != null)
					if (su.equals(annot.getSubject())) {
						annots.add(annot);
					}
			}
		}
		return annots;
	}
	
	public static Collection<Annotation> getAnnotationsForObject(OBOSession session, IdentifiedObject ob) {
		Collection<Annotation> annots = new LinkedList<Annotation>();
		for (IdentifiedObject io : session.getObjects()) {
			if (io instanceof Annotation) {
				Annotation annot = (Annotation)io;
				// Extra defence against null annotations from Phenote
				if (annot.getSubject() != null &&
						annot.getObject() != null)
					if (ob.equals(annot.getObject())) {
						annots.add(annot);
					}
			}
		}
		return annots;
	}
	
	public static Set<OBOObject> getSubjectsAnnotatedWithObject(OBOSession session, IdentifiedObject ob) {
		Collection<Annotation> annots = getAnnotationsForObject(session, ob);
		Set<OBOObject> subjs = new HashSet<OBOObject>();
		for (Annotation annot: annots)
			subjs.add((OBOObject) annot.getSubject());
		return subjs;
	}
	
	public static Collection<Annotation> getAnnotations(OBOSession session) {
		Collection<Annotation> annots = new LinkedList<Annotation>();
		for (IdentifiedObject io : session.getObjects()) {
			if (io instanceof Annotation) {
				Annotation annot = (Annotation)io;
				// Extra defence against null annotations from Phenote
				if (annot.getSubject() != null &&
						annot.getObject() != null)
					annots.add(annot);
			}
		}
		return annots;
	}

	public static Collection<OBOObject> getAnnotationSubjects(OBOSession session) {
		Collection<OBOObject> subjs = new HashSet<OBOObject>();
		for (IdentifiedObject io : session.getObjects()) {
			if (io instanceof Annotation) {
				Annotation annot = (Annotation)io;
				
				// Extra defence against null annotations from Phenote
				if (annot.getSubject() != null &&
						annot.getObject() != null) {
					OBOObject subj = (OBOObject) annot.getSubject();
					subjs.add(subj);
				}
			}
		}
		return subjs;	
	}
	
	/**
	 * warning: we have a horrible terminology clash here
	 * the Annotation model uses 'object' to denote the role played by the
	 * OBOObject in the context of a subject-relation-object statement
	 * 
	 * @param session
	 * @return all OBOObjects that are the object of an annotation
	 */
	public static Collection<OBOObject> getAnnotationObjects(OBOSession session) {
		Collection<OBOObject> objs = new HashSet<OBOObject>();
		for (IdentifiedObject io : session.getObjects()) {
			if (io instanceof Annotation) {
				OBOObject obj = (OBOObject) ((Annotation)io).getObject();
				objs.add(obj);
			}
		}
		return objs;	
	}

}
