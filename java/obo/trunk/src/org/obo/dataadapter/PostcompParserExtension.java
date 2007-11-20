package org.obo.dataadapter;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.obo.dataadapter.OBOConstants.TagMapping;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.NestedValue;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.impl.DefaultOperationModel;
import org.obo.datamodel.impl.OBOClassImpl;
import org.obo.history.HistoryItem;
import org.obo.postcomp.ParseException;
import org.obo.postcomp.PostcompUtil;

public class PostcompParserExtension implements ParserExtension,
		OBOSerializerExtension {

	protected OBOSession session;

	protected DefaultOBOParser parser;

	protected PrintStream stream;

	protected OBOSerializer serializer;

	protected Collection<HistoryItem> items = new LinkedList<HistoryItem>();

	protected Map<OBOClass, Collection<Link>> intersectionMap;

	public IdentifiedObject createObject(String stanza, String id) {
		return null;
	}

	public void setParser(DefaultOBOParser parser) {
		this.parser = parser;
	}

	public void setSession(OBOSession session) {
		this.session = session;
	}

	public void cancel() {
	}

	public void endFileParse(String uri) throws OBOParseException {
	}

	public void endParse() throws OBOParseException {
		DefaultOperationModel model = new DefaultOperationModel();
		model.setSession(session);
		for (HistoryItem item : items) {
			model.apply(item);
		}
		intersectionMap = null;
	}

	public String mapID(String id) {
		if (session.getObject(id) == null && id.contains("^")) {
			try {
				HistoryItem item = PostcompUtil.createPostcompItem(session, id,
						intersectionMap, null, true);
				String postcompItemID = item.getTarget();
				if (session.getObject(postcompItemID) != null)
					return postcompItemID;
				OBOClass obj = (OBOClass) session.getObjectFactory()
						.createObject(postcompItemID, OBOClass.OBO_CLASS, true);
				session.addObject(obj);
				items.add(item);
				return postcompItemID;
			} catch (ParseException ex) {
				ex.printStackTrace();
			}
		}
		return null;
	}

	public void readBangComment(String comment) throws OBOParseException {
	}

	public boolean readTagValue(String tag, String value, NestedValue nv,
			boolean handled) throws OBOParseException {
		return false;
	}

	public void setParseEngine(ParseEngine engine) {
	}

	public void startFileParse(String uri) throws OBOParseException {
	}

	public void startParse() throws OBOParseException {
		items.clear();
		intersectionMap = new HashMap<OBOClass, Collection<Link>>();
	}

	public void startSerialize() throws IOException {
	}

	public boolean startStanza(String name) throws OBOParseException {
		return false;
	}

	public void endSerialize() throws IOException {
	}

	public void setOutputStream(PrintStream stream) throws IOException {
		this.stream = stream;
	}

	public void setSerializer(OBOSerializer serializer) {
		this.serializer = serializer;
	}

	public String mapIDforWrite(IdentifiedObject io, String id) {
		if (PostcompUtil.isPostcompable(io)) {
			OBOClass oboClass = (OBOClass) io;
			return PostcompUtil.getPostcompID(oboClass);
		} else
			return null;
	}

	public boolean writeObject(LinkDatabase linkDatabase, IdentifiedObject obj)
			throws IOException {
		return PostcompUtil.isPostcompable(obj);
	}

	public void changeHeaderTagOrder(List headerTagOrder) {
	}

	public void changeStanzaOrder(List stanzaOrder) {
	}

	public void changeTagOrder(List<TagMapping> tagOrder) {
	}

	public boolean writeTag(TagMapping mapping, IdentifiedObject object,
			LinkDatabase linkDatabase) throws IOException {
		return false;
	}

	public boolean endStanza(IdentifiedObject obj) throws IOException {
		return false;
	}

	public boolean startStanza(IdentifiedObject obj) throws IOException {
		return false;
	}

	public void setSerializationEngine(OBOSerializationEngine engine) {

	}

}
