package org.obo.dataadapter;

import java.io.IOException;
import java.io.PrintStream;
import java.util.List;

import org.obo.dataadapter.OBOConstants.TagMapping;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.OBOSession;

public interface OBOSerializerExtension {
	public void setSerializer(OBOSerializer serializer);

	public void setSession(OBOSession session);
	
	public void setSerializationEngine(OBOSerializationEngine engine);

	public void setOutputStream(PrintStream stream) throws IOException;

	public void startSerialize() throws IOException;

	public void endSerialize() throws IOException;

	public String mapIDforWrite(IdentifiedObject io, String id);

	public void changeTagOrder(List<TagMapping> tagOrder);

	public void changeStanzaOrder(List stanzaOrder);

	public void changeHeaderTagOrder(List headerTagOrder);
	
	public boolean startStanza(IdentifiedObject obj) throws IOException;
	public boolean endStanza(IdentifiedObject obj) throws IOException;

	public boolean writeTag(TagMapping mapping, IdentifiedObject object,
			LinkDatabase linkDatabase) throws IOException;

	public boolean writeObject(LinkDatabase linkDatabase, IdentifiedObject obj)
			throws IOException;
}
