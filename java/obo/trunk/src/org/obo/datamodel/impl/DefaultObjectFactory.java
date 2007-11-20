package org.obo.datamodel.impl;

import org.obo.annotation.datamodel.AnnotationOntology;
import org.obo.annotation.datamodel.impl.AnnotationImpl;
import org.obo.datamodel.*;
import org.obo.history.DefaultHistoryList;
import org.obo.history.HistoryList;

public class DefaultObjectFactory implements ObjectFactory {

	/**
	 * 
	 */
	private static final long serialVersionUID = 4992712121441282799L;
	protected static DefaultObjectFactory factory = new DefaultObjectFactory();

	public static ObjectFactory getFactory() {
		return factory;
	}

	public OBOSession createSession() {
		return new OBOSessionImpl(this);
	}

	public IdentifiedObject createObject(String id, OBOClass type,
			boolean isAnonymousID) {
		IdentifiedObject out;
		if (type.equals(OBOClass.OBO_CLASS))
			out = new OBOClassImpl(id);
		else if (type.equals(OBOClass.OBO_PROPERTY))
			out = new OBOPropertyImpl(id);
		else if (type.equals(OBOClass.OBO_INSTANCE))
			out = new InstanceImpl(id);
		else {
			out = new InstanceImpl(id, type);
		}
		out.setIsAnonymous(isAnonymousID);
		return out;
	}

	public DanglingObject createDanglingObject(String id, boolean isType) {
		if (isType)
			return new DanglingPropertyImpl(id);
		else
			return new DanglingObjectImpl(id);
	}

	public OBORestriction createOBORestriction(LinkedObject child,
			OBOProperty type, LinkedObject parent, boolean implied) {
		OBORestriction restriction = new OBORestrictionImpl(child, type,
				parent, implied);
		return restriction;
	}

	public SynonymCategory createSynonymCategory(String id, String name,
			int scope) {
		return new SynonymCategoryImpl(id, name, scope);
	}

	public TermCategory createCategory(String id, String name) {
		return new TermCategoryImpl(id, name);
	}

	public Namespace createNamespace(String id, String path) {
		return new Namespace(id, path);
	}

	public Synonym createSynonym(String name, int type) {
		return new SynonymImpl(name, type);
	}

	public Dbxref createDbxref(String db, String id, String desc, int type,
			Synonym synonym) {
		return new DbxrefImpl(db, id, desc, type, synonym);
	}

	public NestedValue createNestedValue() {
		return new NestedValueImpl();
	}

	public PropertyValue createPropertyValue(String property, String value) {
		return new PropertyValueImpl(property, value, null, -1);
	}

	public HistoryList createHistoryList() {
		return new DefaultHistoryList();
	}
}
