package org.obo.dataadapter;

import org.obo.datamodel.NestedValue;

public interface OBOSimpleParser {
	
	public String mapID(String id);

	public void startParse() throws OBOParseException;

	public void endParse() throws OBOParseException;

	public void startFileParse(String uri) throws OBOParseException;

	public void endFileParse(String uri) throws OBOParseException;

	public void startStanza(String name) throws OBOParseException;

	public void readBangComment(String comment) throws OBOParseException;

	public void readTagValue(String tag, String value, NestedValue nv, boolean handled)
			throws OBOParseException;

	public void setParseEngine(ParseEngine engine);

	public void cancel();
}
