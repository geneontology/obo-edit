package org.obo.annotation.dataadapter;

import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.obo.annotation.datamodel.Annotation;
import org.obo.annotation.datamodel.AnnotationOntology;
import org.obo.annotation.datamodel.impl.AnnotationImpl;
import org.obo.dataadapter.DefaultOBOParser;
import org.obo.dataadapter.OBOConstants;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.dataadapter.OBOParseException;
import org.obo.dataadapter.OBOSerializationEngine;
import org.obo.dataadapter.OBOSerializer;
import org.obo.dataadapter.OBOSerializerExtension;
import org.obo.dataadapter.OBO_1_2_Serializer;
import org.obo.dataadapter.ParseEngine;
import org.obo.dataadapter.ParserExtension;
import org.obo.dataadapter.OBOConstants.TagMapping;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Instance;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.NestedValue;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBORestriction;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.PropertyValue;
import org.obo.datamodel.UnknownStanza;
import org.obo.datamodel.ValueLink;
import org.obo.datamodel.impl.DanglingObjectImpl;
import org.obo.datamodel.impl.DanglingPropertyImpl;
import org.obo.datamodel.impl.PropertyValueImpl;
import org.obo.util.IDUtil;
import org.obo.util.TermUtil;

public class AnnotationParserExtension implements ParserExtension,
		OBOSerializerExtension {

	protected OBOSession session;

	protected ParseEngine engine;

	protected OBOSerializationEngine sengine;

	protected boolean inAnnotationStanza;

	protected DefaultOBOParser parser;

	protected IdentifiedObject currentObject;

	protected Collection<Annotation> annotations = new LinkedList<Annotation>();

	protected String currentPath;

	protected PrintStream stream;

	protected OBOSerializer serializer;

	protected Annotation currentAnnotation;

	protected static final TagMapping ASSIGNED_BY_TAG = new TagMapping(
			"assigned_by", null, null);

	protected static final TagMapping SUBJECT_TAG = new TagMapping("subject",
			null, null);

	protected static final TagMapping RELATIONSHIP_TAG = new TagMapping(
			"relation", null, null);

	protected static final TagMapping OBJECT_TAG = new TagMapping("object",
			null, null);

	protected static final TagMapping EVIDENCE_TAG = new TagMapping("evidence",
			null, null);

	protected static final TagMapping SOURCE_TAG = new TagMapping("source",
			null, null);

	public AnnotationParserExtension() {
	}

	public String mapID(String id) {
		return null;
	}

	public static void main(String[] args) throws Exception {
		OBOFileAdapter adapter = new OBOFileAdapter();
		OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
		config.getReadPaths().add(
				"/home/jrichter/downloads/ontology/pheno-obd.annotation-obo");
		config.getReadPaths().add(
				"/home/jrichter/downloads/ontology/quality.obo");
		config.setBasicSave(false);
		config.setAllowDangling(true);

		OBOSession session = adapter.doOperation(adapter.READ_ONTOLOGY, config,
				null);
		session.importSession(AnnotationOntology.getSession(), true);
		OBOSerializationEngine sengine = new OBOSerializationEngine();

		sengine.serialize(session, new OBO_1_2_Serializer(),
				"/home/jrichter/test_instances.obo");
		sengine.addSerializerExtension(new AnnotationParserExtension());
		sengine.serialize(session, new OBO_1_2_Serializer(),
				"/home/jrichter/test_annotations.obo");
	}

	public void endParse() throws OBOParseException {
		parser.setAllowDanglingParents(true);

		Collection<Annotation> annotations = new ArrayList<Annotation>();
		for (IdentifiedObject io : session.getObjects()) {
			if (!(io instanceof Annotation))
				continue;
			annotations.add((Annotation) io);
		}		
		
		for (Annotation annotation : annotations) {
			Iterator<PropertyValue> it = annotation.getPropertyValues()
					.iterator();
			while (it.hasNext()) {
				PropertyValue pv = it.next();
				if (pv.getProperty().equals(SOURCE_TAG.getName())) {
					IdentifiedObject source = parser.getObject(pv.getValue());
					if (source == null) {
						// jd-r: this is odd! why are we automatically
						// creating an empty object here rather than
						// creating a dangling identifier?
						// cjm: OK, replaced. How about:
						//source = session.getObjectFactory().createObject(
						//		pv.getValue(), OBOClass.OBO_INSTANCE, false);
						source = session.getObjectFactory().
						  createDanglingObject(pv.getValue(), false);
						source = TermUtil.castToInstance((LinkedObject)source);
						((Instance) source).setType(AnnotationOntology
								.PUBLICATION());
						
					}
					annotation.addSource((LinkedObject) source);
					it.remove();
				} else if (pv.getProperty().equals(ASSIGNED_BY_TAG.getName())) {
					IdentifiedObject ab = parser.getObject(pv.getValue());
					if (ab == null) {
						// jd-r: this is odd! why are we automatically
						// creating an empty object here rather than
						// creating a dangling identifier?
						// cjm: fixed
						ab = session.getObjectFactory().createDanglingObject(
								pv.getValue(), false);
						ab = TermUtil.castToInstance((LinkedObject)ab);
						((Instance) ab).setType(AnnotationOntology.AGENT());
					}
					annotation.setAssignedBy((LinkedObject) ab);
					it.remove();
				} else if (pv.getProperty().equals(SUBJECT_TAG.getName())) {
					IdentifiedObject subject = parser.getObject(pv.getValue());
					if (subject == null) {
						if (parser.getAllowDanglingParents()) {
							subject = new DanglingObjectImpl(pv.getValue());
						} else
							throw new OBOParseException("Unknown subject id "
									+ pv.getValue() + " in annotation "
									+ annotation.getID(), currentPath, engine
									.getCurrentLine(), engine.getLineNum());
					} else if (!(subject instanceof LinkedObject)) {
						throw new OBOParseException("Subject " + pv.getValue()
								+ " in annotation " + annotation.getID()
								+ " is a " + subject.getClass()
								+ " not a LinkedObject", currentPath, engine
								.getCurrentLine(), engine.getLineNum());
					}
					annotation.setSubject((LinkedObject) subject);
					it.remove();
				} else if (pv.getProperty().equals(OBJECT_TAG.getName())) {
					IdentifiedObject object = parser.getObject(pv.getValue());
					if (object == null) {
						if (parser.getAllowDanglingParents()) {
							object = new DanglingObjectImpl(pv.getValue());
						} else
							throw new OBOParseException("Unknown object id "
									+ pv.getValue() + " in annotation "
									+ annotation.getID(), currentPath, engine
									.getCurrentLine(), engine.getLineNum());
					} else if (!(object instanceof LinkedObject)) {
						throw new OBOParseException("Object " + pv.getValue()
								+ " in annotation " + annotation.getID()
								+ " is not a LinkedObject", currentPath, engine
								.getCurrentLine(), engine.getLineNum());
					}
					annotation.setObject((LinkedObject) object);
					it.remove();
				} else if (pv.getProperty().equals(RELATIONSHIP_TAG.getName())) {
					IdentifiedObject object = parser.getObject(pv.getValue());
					if (object == null) {
						if (parser.getAllowDanglingParents())
							object = new DanglingPropertyImpl(pv.getValue());
						else
							throw new OBOParseException("Unknown object id "
									+ pv.getValue() + " in annotation "
									+ annotation.getID(), currentPath, engine
									.getCurrentLine(), engine.getLineNum());
					} else if (!(object instanceof OBOProperty)) {
						throw new OBOParseException("Object " + pv.getValue()
								+ " in annotation " + annotation.getID()
								+ " is not an OBOProperty", currentPath, engine
								.getCurrentLine(), engine.getLineNum());
					}
					annotation.setRelationship((OBOProperty) object);
					it.remove();
				} else if (pv.getProperty().equals(EVIDENCE_TAG.getName())) {
					IdentifiedObject object = parser.getObject(pv.getValue());
					if (object == null) {
						if (parser.getAllowDanglingParents())
							object = new DanglingPropertyImpl(pv.getValue());
						else
							throw new OBOParseException("Unknown object id "
									+ pv.getValue() + " in annotation "
									+ annotation.getID(), currentPath, engine
									.getCurrentLine(), engine.getLineNum());
					} else if (!(object instanceof LinkedObject)) {
						throw new OBOParseException("Object " + pv.getValue()
								+ " in annotation " + annotation.getID()
								+ " is not a LinkedObject", currentPath, engine
								.getCurrentLine(), engine.getLineNum());
					}
					annotation.addEvidence((LinkedObject) object);
					it.remove();
				}
			}
		}
	}

	public boolean readTagValue(String tag, String value, NestedValue nv,
			boolean handled) throws OBOParseException {
		if (inAnnotationStanza) {
			if (tag.equals("id")) {
				String id = parser.mapID(value);
				currentObject = fetchObject(id, nv);
				return true;
			} else {
				if (!engine.getReadIDForCurrentBlock()) {
					currentObject = fetchObject(IDUtil
							.fetchTemporaryID(session), nv);
					currentObject.setIsAnonymous(true);
					// parser.getCurrentUnknownStanza().addPropertyValue(
					// new PropertyValueImpl("id", currentObject.getID(),
					// null, -1), null);
				}
				// try {
				// engine.parseTagValue("Annotation", engine.getCurrentPath(),
				// engine.getLineNum(), engine.getLineNum(), tag,
				// value, nv);
				// return true;
				// } catch (IOException e) {
				// throw new OBOParseException(
				// "IOException while attempting callback to parse engine",
				// engine.getCurrentLine(), engine.getCurrentLine(),
				// engine.getLineNum());
				// }
				// if (tag.equals("is_anonymous")) {
				// currentObject.setIsAnonymous(value.equals("true"));
				// return true;
				// }
			}
		}
		return false;
	}

	public IdentifiedObject fetchObject(String id, NestedValue nv) {
		IdentifiedObject currentObject;
		engine.setReadIDForCurrentBlock(true);
		currentObject = parser.fetchObject(id);
		currentObject.setName(id);
		currentObject.setIDExtension(nv);
		session.addObject(currentObject);
		annotations.add((Annotation) currentObject);
		parser.setCurrentObject(currentObject);
		return currentObject;
	}

	public IdentifiedObject createObject(String stanza, String id) {
		if (stanza.equalsIgnoreCase("annotation"))
			return new AnnotationImpl(id);
		else
			return null;
	}

	public boolean startStanza(String name) throws OBOParseException {
		inAnnotationStanza = name.equalsIgnoreCase("annotation");
		return inAnnotationStanza;
	}

	public void setParseEngine(ParseEngine engine) {
		this.engine = engine;
	}

	public void setSession(OBOSession session) {
		this.session = session;
	}

	public void cancel() {
		session = null;
	}

	public void startParse() throws OBOParseException {
		inAnnotationStanza = false;
	}

	public void endFileParse(String uri) throws OBOParseException {
	}

	public void readBangComment(String comment) throws OBOParseException {
	}

	public void startFileParse(String uri) throws OBOParseException {
		currentPath = uri;
	}

	public void startSerialize() throws IOException {
	}

	public void setParser(DefaultOBOParser parser) {
		this.parser = parser;
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
		return null;
	}

	public boolean writeObject(LinkDatabase linkDatabase, IdentifiedObject obj)
			throws IOException {
		return false;
	}

	public void changeHeaderTagOrder(List headerTagOrder) {
	}

	public void changeStanzaOrder(List stanzaOrder) {
	}

	public void changeTagOrder(List<TagMapping> tagOrder) {
		int index = -1;
		Iterator<TagMapping> it = tagOrder.iterator();
		for (index = 0; it.hasNext(); index++) {
			TagMapping mapping = it.next();
			if (mapping.equals(OBOConstants.ALT_ID_TAG)) {
				index++;
				break;
			}
		}
		tagOrder.add(index++, ASSIGNED_BY_TAG);
		tagOrder.add(index++, SUBJECT_TAG);
		tagOrder.add(index++, RELATIONSHIP_TAG);
		tagOrder.add(index++, OBJECT_TAG);
		tagOrder.add(index++, EVIDENCE_TAG);
		tagOrder.add(index, SOURCE_TAG);
	}

	public boolean endStanza(IdentifiedObject obj) throws IOException {
		currentAnnotation = null;
		return false;
	}

	public boolean startStanza(IdentifiedObject obj) throws IOException {
		if (obj instanceof Instance && obj.getType() != null
				&& obj.getType().equals(AnnotationOntology.ANNOTATION())) {
			Instance instance = (Instance) obj;
			if (instance instanceof Annotation) {
				currentAnnotation = (Annotation) instance;
			} else
				currentAnnotation = new AnnotationImpl(instance);
			stream.print("[Annotation]\n");
			return true;
		} else
			return false;
	}

	public boolean writeTag(TagMapping mapping, IdentifiedObject io,
			LinkDatabase linkDatabase) throws IOException {
		if (currentAnnotation != null) {
			if (mapping.equals(ASSIGNED_BY_TAG)) {
				LinkedObject assignedBy = currentAnnotation.getAssignedBy();
				if (assignedBy != null)
					stream.print("assigned_by: " + assignedBy.getID() + "\n");
				return true;
			} else if (mapping.equals(SUBJECT_TAG)) {
				LinkedObject subject = currentAnnotation.getSubject();
				if (subject != null) {
					stream.print("subject: " + sengine.mapID(subject) + " ! "
							+ subject.getName() + "\n");
				}
				return true;
			} else if (mapping.equals(RELATIONSHIP_TAG)) {
				OBOProperty rel = currentAnnotation.getRelationship();
				if (rel != null) {
					stream.print("relation: " + sengine.mapID(rel) + " ! "
							+ rel.getName() + "\n");
				}
				return true;
			} else if (mapping.equals(OBJECT_TAG)) {
				LinkedObject object = currentAnnotation.getObject();
				if (object != null) {
					stream.print("object: " + sengine.mapID(object) + " ! "
							+ object.getName() + "\n");
				}
				return true;
			} else if (mapping.equals(EVIDENCE_TAG)) {
				for (LinkedObject lo : currentAnnotation.getEvidence()) {
					stream.print("evidence: " + sengine.mapID(lo) + "\n");
				}
				return true;
			} else if (mapping.equals(OBOConstants.INSTANCE_OF_TAG)) {
				return true;
			} else if (mapping.equals(SOURCE_TAG)) {
				for (LinkedObject source : currentAnnotation.getSources()) {
					stream.print("source: " + source.getID() + "\n");
				}
				return true;
			} else if (mapping.equals(OBOConstants.LINK_TAG)
					|| mapping.equals(OBOConstants.VALUE_LINK_TAG)) {
				if (io instanceof LinkedObject) {
					LinkedObject lo = (LinkedObject) io;
					List linkList = new LinkedList();
					linkList.addAll(linkDatabase.getParents(lo));
					Collections.sort(linkList, sengine
							.getLinkComparator(serializer));
					Iterator it2 = linkList.iterator();
					while (it2.hasNext()) {
						Link l = (Link) it2.next();
//						if (l instanceof OBORestriction) {
//							OBORestriction link = (OBORestriction) l;
							if (l.getType().equals(
									AnnotationOntology.ASSIGNED_BY_REL())
									|| l.getType().equals(
											AnnotationOntology.EVIDENCE_REL())
									|| l.getType().equals(
											AnnotationOntology.POSITS_REL())
									|| l.getType().equals(
											AnnotationOntology.SOURCE_REL())) {
								continue;
							}
							else if (l instanceof ValueLink) {
								if (mapping.equals(OBOConstants.VALUE_LINK_TAG))
									serializer.writeValueLinkTag((ValueLink) l,
											l.getNestedValue());
							}
							else {
								if (mapping.equals(OBOConstants.LINK_TAG))
									sengine.writeLink(serializer, l);
							}
//						}
//						else {
//						System.out.println("xx"+l+" "+mapping+" "+l.getClass());
//						}
					}
				}
				return true;
			}
		}
		return false;
	}

	public void setSerializationEngine(OBOSerializationEngine engine) {
		sengine = engine;
	}
}
