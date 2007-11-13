package org.obo.annotation.datamodel;

import java.net.URL;

import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBOSession;
import org.obo.util.TermUtil;

public class AnnotationOntology {
	protected static OBOSession session;

	protected static final String ANNOTATION_TYPE_ID = "oban:annotation";
	protected static final String EVIDENCE_TYPE_ID = "oban:evidence";
	protected static final String PUBLICATION_TYPE_ID = "oban:publication";
	
	protected static final String AGENT_TYPE_ID = "oban:agent";
	protected static final String ASSIGNED_BY_PROPERTY_ID = "oban:assigned_by";
	protected static final String SOURCE_PROPERTY_ID = "oban:has_data_source";
	protected static final String EVIDENCE_PROPERTY_ID = "oban:has_evidence";
	protected static final String POSITS_PROPERTY_ID = "oban:posits";
	protected static final String IS_NEGATED = "oban:is_negated";
	

	public static OBOClass ANNOTATION() {
		return (OBOClass) getSession().getObject(ANNOTATION_TYPE_ID);
	}

	public static OBOClass EVIDENCE() {
		return (OBOClass) getSession().getObject(EVIDENCE_TYPE_ID);
	}

	public static OBOProperty ASSIGNED_BY_REL() {
		return (OBOProperty) getSession().getObject(ASSIGNED_BY_PROPERTY_ID);
	}

	public static OBOProperty SOURCE_REL() {
		return (OBOProperty) getSession().getObject(SOURCE_PROPERTY_ID);
	}

	public static OBOProperty EVIDENCE_REL() {
		return (OBOProperty) getSession().getObject(EVIDENCE_PROPERTY_ID);
	}

	public static OBOProperty POSITS_REL() {
		return (OBOProperty) getSession().getObject(POSITS_PROPERTY_ID);
	}
	
	public static OBOProperty IS_NEGATED() {
		return (OBOProperty) getSession().getObject(IS_NEGATED);
	}
	
	public static OBOClass PUBLICATION() {
		return (OBOClass) getSession().getObject(PUBLICATION_TYPE_ID);
	}
	
	public static OBOClass AGENT() {
		return (OBOClass) getSession().getObject(AGENT_TYPE_ID);
	}


	public static OBOSession getSession() {
		if (session == null) {
			session = loadSession();
		}
		return session;
	}

	protected static OBOSession loadSession() {
		try {
			URL url = AnnotationOntology.class.getResource(
					"/org/obo/annotation/resources/oban.obo");
			return TermUtil.getSession(url.toString());
		} catch (Exception ex) {
			return null;
		}
	}
}
