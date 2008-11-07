package org.obo.datamodel;

import java.io.Serializable;

import org.obo.history.HistoryList;

public interface ObjectFactory extends Serializable, Cloneable {

	public HistoryList createHistoryList();

	public OBOSession createSession();

	public IdentifiedObject createObject(String id, OBOClass type,
			boolean isTemporaryID);

	public DanglingObject createDanglingObject(String id, boolean isProperty);

	public OBORestriction createOBORestriction(LinkedObject child,
			OBOProperty type, LinkedObject parent, boolean implied);

	public SynonymType createSynonymType(String id, String name,
			int scope);

	public TermSubset createSubset(String id, String name);

	public Namespace createNamespace(String id, String path);

	public Synonym createSynonym(String name, int type);

	public Dbxref createDbxref(String db, String id, String desc, int type,
			Synonym synonym);

	public NestedValue createNestedValue();

	public PropertyValue createPropertyValue(String property, String value);
}
