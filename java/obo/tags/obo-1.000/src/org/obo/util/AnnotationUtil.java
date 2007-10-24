package org.obo.util;

import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;

import org.obo.annotation.datamodel.Annotation;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOSession;

public class AnnotationUtil {

	public static Collection<Annotation> getAnnotationsForSubject(OBOSession session, IdentifiedObject su) {
		Collection<Annotation> annots = new LinkedList<Annotation>();
		for (IdentifiedObject io : session.getObjects()) {
			if (io instanceof Annotation) {
				Annotation annot = (Annotation)io;
				if (su.equals(annot.getSubject())) {
					annots.add(annot);
				}
			}
		}
		return annots;
	}
	
	public static Collection<Annotation> getAnnotations(OBOSession session) {
		Collection<Annotation> annots = new LinkedList<Annotation>();
		for (IdentifiedObject io : session.getObjects()) {
			if (io instanceof Annotation) {
				annots.add((Annotation)io);
			}
		}
		return annots;
	}

	public static Collection<LinkedObject> getAnnotationSubjects(OBOSession session) {
		Collection<LinkedObject> subjs = new HashSet<LinkedObject>();
		for (IdentifiedObject io : session.getObjects()) {
			if (io instanceof Annotation) {
				LinkedObject subj = ((Annotation)io).getSubject();
				subjs.add(subj);
			}
		}
		return subjs;	
	}
	
	public static Collection<LinkedObject> getAnnotationObjects(OBOSession session) {
		Collection<LinkedObject> objs = new HashSet<LinkedObject>();
		for (IdentifiedObject io : session.getObjects()) {
			if (io instanceof Annotation) {
				LinkedObject obj = ((Annotation)io).getObject();
				objs.add(obj);
			}
		}
		return objs;	
	}

}
