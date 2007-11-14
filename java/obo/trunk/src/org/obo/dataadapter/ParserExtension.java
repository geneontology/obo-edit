package org.obo.dataadapter;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.OBOSession;

public interface ParserExtension extends OBOSimpleParser {	
	public void setSession(OBOSession session);
	public IdentifiedObject createObject(String stanza, String id);
	public void setParser(DefaultOBOParser parser);
}
