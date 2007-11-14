package org.obo.dataadapter;

import java.util.*;
import java.net.*;
import java.io.*;

import org.bbop.io.*;
import org.obo.annotation.dataadapter.AnnotationParserExtension;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.obo.filters.*;
import org.obo.identifier.IDProfile;
import org.obo.identifier.IDRule;
import org.obo.util.IDUtil;

public class DefaultOBOParser implements OBOParser {

	protected ParseEngine engine;

	protected Set<RelStruct> linkSet;

	protected Set<String> pathSet;

	protected Set<PropertyValStruct> propertyValSet;

	protected Set<BasicMapping> considerSet;

	protected Set<BasicMapping> useSet;

	protected Map<IdentifiedObject, String> rangeMap;

	protected Map<IdentifiedObject, String> domainMap;

	protected Map<String, Namespace> namespaceMap;

	protected Map<String, InstanceStruct> instanceOfHash;

	protected Map<String, String> idMapping;

	protected String idPrefix;

	protected Stack<Namespace> namespaceStack;

	protected Stack<String> pathStack;

	protected List<UnknownStanza> unknownStanzaList;

	protected IdentifiedObject currentObject;

	protected UnknownStanza unknownStanza;

	protected String currentStanza;

	protected OBOSession session;

	protected boolean requireSubsumption = false;

	protected boolean allowDanglingParents = true;

	protected ObjectFactory objectFactory = new DefaultObjectFactory();

	protected boolean halted = false;

	protected IDProfile currentProfile;

	protected Collection<ParserExtension> parserExtensions = new LinkedList<ParserExtension>();

	protected OBOMetaData metaData;

	protected boolean failFast = false;

	protected static class BasicMapping {
		protected String subject;

		protected String object;

		protected NestedValue nv;

		protected String path;

		protected String line;

		protected int linenum;

		public BasicMapping(String subject, String object, NestedValue nv,
				String path, String line, int linenum) {
			this.subject = subject;
			this.object = object;
			this.nv = nv;
			this.line = line;
			this.linenum = linenum;
		}

		public String getPath() {
			return path;
		}

		public String getLine() {
			return line;
		}

		public int getLineNum() {
			return linenum;
		}

		public String getSubject() {
			return subject;
		}

		public String getObject() {
			return object;
		}

		public NestedValue getNestedValue() {
			return nv;
		}
	}

	protected static class InstanceStruct {
		protected String instanceOf;

		protected NestedValue nv;

		protected String path;

		protected String line;

		protected int linenum;

		public InstanceStruct(String instanceOf, NestedValue nv, String path,
				String line, int linenum) {
			this.instanceOf = instanceOf;
			this.path = path;
			this.line = line;
			this.linenum = linenum;
			this.nv = nv;
		}

		public String getPath() {
			return path;
		}

		public String getLine() {
			return line;
		}

		public int getLineNum() {
			return linenum;
		}
	}

	protected static class PropertyValStruct {
		protected String instanceID;

		protected String propID;

		protected String val;

		protected String typeID;

		protected NestedValue nv;

		protected String line;

		protected int linenum;

		protected String path;

		protected boolean quoted;

		public PropertyValStruct(String instanceID, String propID, String val,
				String typeID, NestedValue nv, String path, boolean quoted,
				String line, int linenum) {
			this.instanceID = instanceID;
			this.propID = propID;
			this.val = val;
			this.typeID = typeID;
			this.nv = nv;
			this.path = path;
			this.quoted = quoted;
			this.line = line;
			this.linenum = linenum;
		}

		public boolean isQuoted() {
			return quoted;
		}

		public String getPath() {
			return path;
		}

		public String getLine() {
			return line;
		}

		public int getLineNum() {
			return linenum;
		}
	}

	protected static class RelStruct implements Comparable {
		protected String child;

		protected String parent;

		protected String type;

		protected int linenum;

		protected String path;

		protected String line;

		protected String ns;

		protected NestedValue nv;

		protected boolean necessary = true;

		protected boolean inverseNecessary = false;

		protected boolean completes = false;

		protected boolean implied = false;

		protected Integer minCardinality;

		protected Integer maxCardinality;

		protected Integer cardinality;

		public int compareTo(Object b) {
			if (b instanceof RelStruct) {
				if (linenum < ((RelStruct) b).getLineNum())
					return -1;
				else if (linenum > ((RelStruct) b).getLineNum())
					return 1;
				else
					return 0;
			} else
				return 1;
		}

		/*
		 * public RelStruct(String child, String parent, String type, int
		 * linenum, String line) { this(child, parent, type, linenum, line,
		 * false, false, null); }
		 */
		public RelStruct(String child, String parent, String type, String path,
				int linenum, String line, boolean necessary,
				boolean inverseNecessary, boolean completes, boolean implied,
				Integer minCardinality, Integer maxCardinality,
				Integer cardinality, String ns, NestedValue nv) {
			this.child = child;
			this.parent = parent;
			this.type = type;
			this.linenum = linenum;
			this.path = path;
			this.line = line;
			this.necessary = necessary;
			this.inverseNecessary = inverseNecessary;
			this.completes = completes;
			this.implied = implied;
			this.cardinality = cardinality;
			this.maxCardinality = maxCardinality;
			this.minCardinality = minCardinality;
			this.nv = nv;
			this.ns = ns;
		}

		public String getPath() {
			return path;
		}

		public Integer getMinCardinality() {
			return minCardinality;
		}

		public Integer getMaxCardinality() {
			return maxCardinality;
		}

		public Integer getCardinality() {
			return cardinality;
		}

		public void setNestedValue(NestedValue nv) {
			this.nv = nv;
		}

		public NestedValue getNestedValue() {
			return nv;
		}

		public String getNamespace() {
			return ns;
		}

		public boolean isNecessary() {
			return necessary;
		}

		public boolean isInverseNecessary() {
			return inverseNecessary;
		}

		public boolean isImplied() {
			return implied;
		}

		public boolean completes() {
			return completes;
		}

		public int getLineNum() {
			return linenum;
		}

		public String getLine() {
			return line;
		}

		public String getChild() {
			return child;
		}

		public String getParent() {
			return parent;
		}

		public String getType() {
			return type;
		}
	}

	public DefaultOBOParser() {
		instanceOfHash = new HashMap<String, InstanceStruct>();
		idMapping = new HashMap<String, String>();
		idPrefix = null;
		propertyValSet = new HashSet<PropertyValStruct>();
		unknownStanzaList = new Vector<UnknownStanza>();
		linkSet = new HashSet<RelStruct>();
		pathSet = new HashSet<String>();
		namespaceStack = new Stack<Namespace>();
		pathStack = new Stack<String>();
		namespaceMap = new HashMap<String, Namespace>();
		rangeMap = new HashMap<IdentifiedObject, String>();
		domainMap = new HashMap<IdentifiedObject, String>();
		considerSet = new HashSet<BasicMapping>();
		useSet = new HashSet<BasicMapping>();
		addParserExtension(new AnnotationParserExtension());
		addParserExtension(new PostcompParserExtension());
		// currentProfile = new NamedIDProfile("<default id profile>");
	}

	public void cancel() {
		halted = true;
		for (ParserExtension extension : parserExtensions) {
			extension.cancel();
		}
	}

	public ObjectFactory getObjectFactory() {
		return objectFactory;
	}

	public void setObjectFactory(ObjectFactory factory) {
		this.objectFactory = factory;
	}

	protected Namespace getDefaultNamespace() {
		Namespace out = (Namespace) namespaceStack.peek();
		return out;
	}

	public void setRequireSubsumption(boolean requireSubsumption) {
		this.requireSubsumption = requireSubsumption;
	}

	public void setAllowDanglingParents(boolean allowDanglingParents) {
		this.allowDanglingParents = allowDanglingParents;
	}

	public boolean getAllowDanglingParents() {
		return allowDanglingParents;
	}

	public OBOSession getSession() {
		return session;
	}

	public void addParserExtension(ParserExtension extension) {
		parserExtensions.add(extension);
		extension.setParser(this);
	}

	public void removeParserExtension(ParserExtension extension) {
		parserExtensions.remove(extension);
		extension.setParser(null);
	}

	public void readNamespaceIDRule(String ns, String rule) {
		if (currentProfile == null)
			currentProfile = new NamedIDProfile("<default id profile>");

		if (ns == null) {
			currentProfile.setDefaultRule(rule);
		} else {
			IDRule idRule = new DefaultIDRule();
			ObjectFilter filter = new ObjectFilterImpl();
			filter.setAspect(new SelfSearchAspect());
			filter.setCriterion(new NamespaceSearchCriterion());
			filter.setComparison(new EqualsComparison());
			filter.setValue(ns);
			CompoundFilter cfilter = new CompoundFilterImpl();
			cfilter.addFilter(filter);
			idRule.setFilter(cfilter);
			idRule.setRule(rule);
			currentProfile.addRule(idRule);
		}
	}

	public void readFormatVersion(String version) {
		// ignore it
	}

	public String getCurrentPath() {
		return (String) pathStack.peek();
	}

	public void readImport(String path) throws IOException, OBOParseException {
		URL originalURL = IOUtil.getURL(getCurrentPath());
		try {
			URL url = new URL(originalURL, path);
			path = url.toString();
			if (!pathSet.contains(path)) {
				metaData.addImport(getCurrentPath(), path);
				engine.parse(path);
			}
		} catch (MalformedURLException ex) {
			// throw new OBOParseException("Bad import URL:
			// "+originalURL+"/"+path);
			ex.printStackTrace();
		}
	}

	public void readFileVersion(String version) {
		metaData.mapFileData(getCurrentPath(), version);
	}

	public void readDate(Date date) {
		session.getCurrentHistory().setDate(date);
	}

	public void readSavedBy(String savedBy) {
		session.getCurrentHistory().setUser(savedBy);
	}

	public void readAutogeneratedBy(String autogeneratedBy) {
		// ignore it
	}

	public void readRemark(String remark) {
		remark = remark.trim();
		if (session.getCurrentHistory().getComment() == null)
			session.getCurrentHistory().setComment(remark);
		else {
			StringTokenizer tokenizer = new StringTokenizer(session
					.getCurrentHistory().getComment(), "\n");
			while (tokenizer.hasMoreTokens()) {
				String token = tokenizer.nextToken();
				if (token.equals(remark))
					return;
			}
			session.getCurrentHistory().setComment(
					session.getCurrentHistory().getComment() + "\n" + remark);
		}
	}

	public void readSubsetDef(String name, String desc) {
		TermCategory cat = objectFactory.createCategory(name, desc);
		session.addCategory(cat);
	}

	public void readSynonymCategory(String id, String name, int scope) {
		SynonymCategory cat = objectFactory.createSynonymCategory(id, name,
				scope);
		// new SynonymCategoryImpl(id, name, scope);
		session.addSynonymCategory(cat);
	}

	public IdentifiedObject getObject(String id) {
		IdentifiedObject io = session.getObject(mapID(id));
		return io;
	}

	protected static boolean isBuiltInID(String id) {
		for (int i = 0; i < OBOProperty.BUILTIN_TYPES.length; i++)
			if (OBOProperty.BUILTIN_TYPES[i].getID().equals(id))
				return true;
		return false;
	}

	public String mapID(String id) {

		if (idPrefix != null && id.indexOf(':') == -1 && !isBuiltInID(id))
			id = idPrefix + ":" + id;
		String newid = (String) idMapping.get(id);
		if (newid != null)
			id = newid;
		for (ParserExtension extension : parserExtensions) {
			String mangledID = extension.mapID(id);
			if (mangledID != null)
				id = mangledID;
		}
		return id;
	}

	public IdentifiedObject fetchObject(String id) {
		IdentifiedObject out = getObject(id);
		if (out == null) {
			out = createObject(currentStanza, id);
			session.addObject(out);
		}
		return out;
	}
	
	protected IdentifiedObject createObject(String currentStanza, String id) {
		IdentifiedObject out = null;
		for (ParserExtension extension : parserExtensions) {
			out = extension.createObject(currentStanza, id);
			if (out != null)
				break;
		}
		if (out == null) {
			if (currentStanza.equalsIgnoreCase("typedef"))
				out = objectFactory.createObject(id, OBOClass.OBO_PROPERTY,
						false);
			else if (currentStanza.equalsIgnoreCase("term"))
				out = objectFactory.createObject(id, OBOClass.OBO_CLASS,
						false);
			else if (currentStanza.equalsIgnoreCase("instance"))
				out = objectFactory.createObject(id, OBOClass.OBO_INSTANCE,
						false);
		}
		return out;
	}

	// go here
	public void readIDMapping(String originalid, String newid)
			throws OBOParseException {
		if (idMapping.containsKey(originalid)
				&& !idMapping.get(originalid).equals(newid)) {
			throw new OBOParseException("Multiple mappings assigned to "
					+ originalid, getCurrentPath(), engine.getCurrentLine(),
					engine.getLineNum());
		}
		idMapping.put(originalid, newid);
	}

	public void readIDPrefix(String prefix) throws OBOParseException {
		if (idPrefix != null && !idPrefix.equals(prefix))
			throw new OBOParseException("Multiple id-prefixes defined",
					getCurrentPath(), engine.getCurrentLine(), engine
							.getLineNum(), 0);
		idPrefix = prefix;
		// create id mappings for every built-in object
		Iterator it = session.getObjects().iterator();
		while (it.hasNext()) {
			IdentifiedObject io = (IdentifiedObject) it.next();
			String convertedID = mapID(io.getID());
			if (!convertedID.equals(io.getID())) {
				idMapping.put(convertedID, io.getID());
			}
		}
	}

	public void readID(String id, NestedValue nv) {
		id = mapID(id);
		currentObject = fetchObject(id);
		currentObject.setIDExtension(nv);
		session.addObject(currentObject);
	}

	public void readName(String name, NestedValue nv) {
		currentObject.setName(name);
		currentObject.setNameExtension(nv);
	}

	public void readRange(String range, NestedValue nv)
			throws OBOParseException {
		if (!(currentObject instanceof OBOProperty))
			throw new OBOParseException("Attempt to set range of "
					+ "non-type " + currentObject + ".", getCurrentPath(),
					engine.getCurrentLine(), engine.getLineNum());
		rangeMap.put(currentObject, range);
	}

	public void readDomain(String domain, NestedValue nv)
			throws OBOParseException {
		if (!(currentObject instanceof OBOProperty))
			throw new OBOParseException("Attempt to set domain of "
					+ "non-type " + currentObject + ".", getCurrentPath(),
					engine.getCurrentLine(), engine.getLineNum());
		domainMap.put(currentObject, domain);
	}

	public void readAltID(String id, NestedValue nv) throws OBOParseException {
		if (!(currentObject instanceof MultiIDObject))
			throw new OBOParseException("Attempted to add secondary id to "
					+ "object " + currentObject
					+ " which does not support secondary " + "ids.",
					getCurrentPath(), engine.getCurrentLine(), engine
							.getLineNum());
		((MultiIDObject) currentObject).addSecondaryID(id);
		if (nv != null)
			((MultiIDObject) currentObject).addSecondaryIDExtension(id, nv);
	}

	public void readComment(String comment, NestedValue nv)
			throws OBOParseException {
		if (!(currentObject instanceof CommentedObject))
			throw new OBOParseException("Attempted to set comment of "
					+ "object " + currentObject
					+ " which does not support comments.", getCurrentPath(),
					engine.getCurrentLine(), engine.getLineNum());
		((CommentedObject) currentObject).setComment(comment);
		((CommentedObject) currentObject).setCommentExtension(nv);
	}

	public void readInstanceOf(String termID, NestedValue nv)
			throws OBOParseException {
		if (!(currentObject instanceof Instance))
			throw new OBOParseException("Attempted to set instance_of "
					+ "value for non-instance " + currentObject,
					getCurrentPath(), engine.getCurrentLine(), engine
							.getLineNum());

		InstanceStruct is = new InstanceStruct(termID, nv, getCurrentPath(),
				engine.getCurrentLine(), engine.getLineNum());
		instanceOfHash.put(mapID(currentObject.getID()), is);
	}

	public void readPropertyValue(String propID, String val, String typeID,
			boolean quoted, NestedValue nv) throws OBOParseException {

		if (!(currentObject instanceof Instance))
			throw new OBOParseException("Attempted to set instance_of "
					+ "value for non-instance " + currentObject,
					getCurrentPath(), engine.getCurrentLine(), engine
							.getLineNum());

		PropertyValStruct pvs = new PropertyValStruct(currentObject.getID(),
				propID, val, typeID, nv, getCurrentPath(), quoted, engine
						.getCurrentLine(), engine.getLineNum());
		propertyValSet.add(pvs);
	}

	protected Synonym getSynonym(String name, XrefPair[] xrefs, int type,
			String catID, NestedValue nv) throws OBOParseException {
		Synonym s = objectFactory.createSynonym(name, type);
		s.setNestedValue(nv);
		if (catID != null) {
			SynonymCategory synCat = session.getSynonymCategory(catID);
			if (synCat == null)
				throw new OBOParseException("Unrecognized category id " + catID
						+ " found", getCurrentPath(), engine.getCurrentLine(),
						engine.getLineNum());
			s.setSynonymCategory(synCat);
		}
		for (int i = 0; i < xrefs.length; i++) {
			Dbxref ref = getDbxref(xrefs[i], Dbxref.RELATED_SYNONYM);
			s.addDbxref(ref);
		}
		return s;
	}

	protected Dbxref getDbxref(XrefPair pair, int type) {
		String dbx = pair.xref;
		int index = dbx.indexOf(':');
		String id;
		String db;
		if (index < 0) {
			db = "";
			id = dbx;
		} else {
			db = dbx.substring(0, index);
			id = dbx.substring(index + 1, dbx.length());
		}
		Dbxref ref = objectFactory.createDbxref(db, id, pair.desc, type, null);
		ref.setNestedValue(pair.nv);
		return ref;
	}

	public void readDef(String def, XrefPair[] xrefs, NestedValue nv)
			throws OBOParseException {
		if (!(currentObject instanceof DefinedObject))
			throw new OBOParseException("Attempted to set definition of "
					+ "object " + currentObject + " which does not support "
					+ "definitions.", getCurrentPath(),
					engine.getCurrentLine(), engine.getLineNum());

		((DefinedObject) currentObject).setDefinition(def);
		for (int i = 0; i < xrefs.length; i++) {
			Dbxref ref = getDbxref(xrefs[i], Dbxref.DEFINITION);
			((DefinedObject) currentObject).addDefDbxref(ref);
		}
		((DefinedObject) currentObject).setDefinitionExtension(nv);
	}

	public void readXrefAnalog(XrefPair xref) throws OBOParseException {
		if (!(currentObject instanceof DbxrefedObject))
			throw new OBOParseException("Attempted to add dbxref to "
					+ "object " + currentObject
					+ " which does not support dbxrefs.", getCurrentPath(),
					engine.getCurrentLine(), engine.getLineNum());

		Dbxref ref = getDbxref(xref, Dbxref.ANALOG);
		((DbxrefedObject) currentObject).addDbxref(ref);
	}

	public void readXrefUnk(XrefPair xref) throws OBOParseException {
		if (!(currentObject instanceof DbxrefedObject))
			throw new OBOParseException("Attempted to add dbxref to "
					+ "object " + currentObject
					+ " which does not support dbxrefs.", getCurrentPath(),
					engine.getCurrentLine(), engine.getLineNum());
		Dbxref ref = getDbxref(xref, Dbxref.UNKNOWN);
		((DbxrefedObject) currentObject).addDbxref(ref);
	}

	public void readSubset(String name, NestedValue nv)
			throws OBOParseException {
		if (!(currentObject instanceof CategorizedObject)) {
			throw new OBOParseException("Attempted to add category to "
					+ "object " + currentObject
					+ " which does not support categories.", getCurrentPath(),
					engine.getCurrentLine(), engine.getLineNum());
		}
		TermCategory cat = session.getCategory(name);

		if (cat == null)
			throw new OBOParseException("Undefined category " + name + ".",
					getCurrentPath(), engine.getCurrentLine(), engine
							.getLineNum());

		((CategorizedObject) currentObject).addCategory(cat);
		((CategorizedObject) currentObject).addCategoryExtension(cat, nv);
	}

	public void readSynonym(String name, XrefPair[] xrefs, int scope,
			String catID, NestedValue nv) throws OBOParseException {
		if (!(currentObject instanceof SynonymedObject))
			throw new OBOParseException("Attempted to add synonym to "
					+ "object " + currentObject
					+ " which does not support synonyms.", getCurrentPath(),
					engine.getCurrentLine(), engine.getLineNum());
		((SynonymedObject) currentObject).addSynonym(getSynonym(name, xrefs,
				scope, catID, nv));
	}

	public void readRelationship(String rel_type, String id, boolean necessary,
			boolean inverseNecessary, boolean completes, boolean implied,
			Integer minCardinality, Integer maxCardinality,
			Integer cardinality, String ns, NestedValue nv)
			throws OBOParseException {
		if (!(currentObject instanceof LinkedObject))
			throw new OBOParseException("Tried to specify relationship "
					+ "for object " + currentObject + " which "
					+ "does not support relationships.", getCurrentPath(),
					engine.getCurrentLine(), engine.getLineNum());

		linkSet.add(new RelStruct(mapID(currentObject.getID()), mapID(id),
				mapID(rel_type), getCurrentPath(), engine.getLineNum(), engine
						.getCurrentLine(), necessary, inverseNecessary,
				completes, implied, minCardinality, maxCardinality,
				cardinality, ns, nv));
	}

	public void readIsa(String id, String ns, boolean completes,
			boolean implied, NestedValue nv) throws OBOParseException {
		if (!(currentObject instanceof LinkedObject))
			throw new OBOParseException("Tried to specify isa " + "for object "
					+ currentObject + " which "
					+ "does not support relationships.", getCurrentPath(),
					engine.getCurrentLine(), engine.getLineNum());

		linkSet.add(new RelStruct(mapID(currentObject.getID()), mapID(id),
				OBOProperty.IS_A.getID(), getCurrentPath(),
				engine.getLineNum(), engine.getCurrentLine(), true, false,
				completes, implied, null, null, null, ns, nv));
	}

	public void readDisjoint(String id, String ns, boolean implied,
			NestedValue nv) throws OBOParseException {
		if (!(currentObject instanceof LinkedObject))
			throw new OBOParseException("Tried to specify disjoint_from "
					+ "for object " + currentObject + " which "
					+ "does not support relationships.", getCurrentPath(),
					engine.getCurrentLine(), engine.getLineNum());

		linkSet.add(new RelStruct(mapID(currentObject.getID()), mapID(id),
				OBOProperty.DISJOINT_FROM.getID(), getCurrentPath(), engine
						.getLineNum(), engine.getCurrentLine(), true, false,
				false, implied, null, null, null, ns, nv));
	}

	public void readInverseOf(String id, String ns, boolean implied,
			NestedValue nv) throws OBOParseException {
		if (!(currentObject instanceof LinkedObject))
			throw new OBOParseException("Tried to specify inverse_of "
					+ "for object " + currentObject + " which "
					+ "does not support relationships.", getCurrentPath(),
					engine.getCurrentLine(), engine.getLineNum());

		linkSet.add(new RelStruct(mapID(currentObject.getID()), mapID(id),
				OBOProperty.INVERSE_OF.getID(), getCurrentPath(), engine
						.getLineNum(), engine.getCurrentLine(), true, false,
				false, implied, null, null, null, ns, nv));
	}

	public void readIsCyclic(boolean isCyclic, NestedValue nv)
			throws OBOParseException {
		if (currentObject instanceof OBOProperty) {
			((OBOProperty) currentObject).setCyclic(isCyclic);
			((OBOProperty) currentObject).setCyclicExtension(nv);
		} else
			throw new OBOParseException("Attempt to set cyclic attribute of "
					+ "non-property " + currentObject + ".", getCurrentPath(),
					engine.getCurrentLine(), engine.getLineNum());
	}

	public void readIsTransitive(boolean isTransitive, NestedValue nv)
			throws OBOParseException {
		if (currentObject instanceof OBOProperty) {
			((OBOProperty) currentObject).setTransitive(isTransitive);
			((OBOProperty) currentObject).setTransitiveExtension(nv);
		} else
			throw new OBOParseException("Attempt to set transitive "
					+ "attribute of non-type " + currentObject + ".",
					getCurrentPath(), engine.getCurrentLine(), engine
							.getLineNum());
	}

	public void readIsSymmetric(boolean isSymmetric, NestedValue nv)
			throws OBOParseException {
		if (currentObject instanceof OBOProperty) {
			((OBOProperty) currentObject).setSymmetric(isSymmetric);
			((OBOProperty) currentObject).setSymmetricExtension(nv);
		} else
			throw new OBOParseException("Attempt to set symmetric "
					+ "attribute of non-type " + currentObject + ".",
					getCurrentPath(), engine.getCurrentLine(), engine
							.getLineNum());
	}

	public void readAlwaysImpliesInverse(boolean b, NestedValue nv)
			throws OBOParseException {
		if (currentObject instanceof OBOProperty) {
			((OBOProperty) currentObject).setAlwaysImpliesInverse(b);
			((OBOProperty) currentObject).setAlwaysImpliesInverseExtension(nv);
		} else
			throw new OBOParseException("Attempt to set always_implies_inverse "
					+ "attribute of non-type " + currentObject + ".",
					getCurrentPath(), engine.getCurrentLine(), engine
							.getLineNum());
	}

	public void readIsReflexive(boolean b, NestedValue nv)
			throws OBOParseException {
		if (currentObject instanceof OBOProperty) {
			((OBOProperty) currentObject).setReflexive(b);
			((OBOProperty) currentObject).setReflexiveExtension(nv);
		} else
			throw new OBOParseException("Attempt to set reflexive "
					+ "attribute of non-type " + currentObject + ".",
					getCurrentPath(), engine.getCurrentLine(), engine
							.getLineNum());
	}

	public void readIsAnonymous(NestedValue nv) {
		currentObject.setIsAnonymous(true);
		currentObject.setAnonymousExtension(nv);
	}

	public void readIsObsolete(NestedValue nv) throws OBOParseException {
		if (currentObject instanceof ObsoletableObject) {
			((ObsoletableObject) currentObject).setObsolete(true);
			((ObsoletableObject) currentObject).setObsoleteExtension(nv);
		} else
			throw new OBOParseException("Attempt to obsolete "
					+ "non-obsoletable object " + currentObject + ".",
					getCurrentPath(), engine.getCurrentLine(), engine
							.getLineNum());
	}

	public void readReplacedBy(String id, NestedValue nv) {
		useSet
				.add(new BasicMapping(currentObject.getID(), id, nv,
						getCurrentPath(), engine.getCurrentLine(), engine
								.getLineNum()));
	}

	public void readConsider(String id, NestedValue nv) {
		considerSet
				.add(new BasicMapping(currentObject.getID(), id, nv,
						getCurrentPath(), engine.getCurrentLine(), engine
								.getLineNum()));
	}

	public void startParse() throws OBOParseException {
		halted = false;
		linkSet.clear();
		pathSet.clear();
		rangeMap.clear();
		domainMap.clear();
		unknownStanzaList.clear();
		namespaceMap.clear();
		useSet.clear();
		considerSet.clear();
		currentProfile = null;
		session = objectFactory.createSession();
		metaData = new OBOMetaData();
		for (ParserExtension extension : parserExtensions) {
			extension.setSession(session);
			extension.setParseEngine(engine);
			extension.startParse();
		}
	}

	public void readDefaultNamespace(String ns) throws OBOParseException {
		// don't add the default namespace to the metadata here, since this
		// method modifies the default namespace already created in
		// startFileParse
		Namespace n = getDefaultNamespace();
		namespaceMap.remove(n.getID());

		Namespace mapped = (Namespace) namespaceMap.get(ns);

		if (mapped != null) {
			namespaceStack.pop();
			namespaceStack.push(mapped);
		} else {
			n.setID(ns);
			namespaceMap.put(n.getID(), n);
		}
	}

	public void readNamespace(String ns, NestedValue nv) {
		Namespace n = (Namespace) namespaceMap.get(ns);
		if (n == null) {
			Namespace def = getDefaultNamespace();
			n = objectFactory.createNamespace(ns, def.getPath());
			session.addNamespace(n);
		}
		metaData.addNamespace(getCurrentPath(), n);
		currentObject.setNamespace(n);
	}

	public void startFileParse(String uri) throws OBOParseException {
		pathSet.add(uri);
		Namespace ns = (Namespace) namespaceMap.get(uri);
		if (ns == null) {
			ns = objectFactory.createNamespace(uri, uri);
			namespaceMap.put(uri, ns);
			session.addNamespace(ns);
		}
		metaData.mapFileData(uri, null);
		metaData.addNamespace(uri, ns);
		namespaceStack.push(ns);
		pathStack.push(uri);
		for (ParserExtension extension : parserExtensions) {
			extension.startFileParse(uri);
		}
	}

	public void endFileParse(String uri) throws OBOParseException {
		if (currentObject != null && currentObject.getNamespace() == null
				&& getDefaultNamespace() != null)
			currentObject.setNamespace(getDefaultNamespace());
		session.setDefaultNamespace((Namespace) namespaceStack.pop());
		idMapping.clear();
		idPrefix = null;
		pathStack.pop();
		currentStanza = null;
		for (ParserExtension extension : parserExtensions) {
			extension.endFileParse(uri);
		}
	}

	public void endParse() throws OBOParseException {
		Iterator it = linkSet.iterator();
		List danglingViolations = new ArrayList();
		while (it.hasNext()) {
			if (halted)
				throw new OBOParseException("Operation cancelled " + "by user",
						null, null, -1);

			RelStruct rs = (RelStruct) it.next();
			IdentifiedObject child = getObject(rs.getChild());
			IdentifiedObject parent = getObject(rs.getParent());
			IdentifiedObject type = getObject(rs.getType());

			if (parent == null) {
				if (allowDanglingParents) {
					parent = objectFactory.createDanglingObject(rs.getParent(),
							false);
					session.addObject(parent);
					System.err.println("added dangling object " + parent);
				} else {
					danglingViolations.add(rs);
					continue;
				}
			} else if (!(parent instanceof LinkedObject))
				throw new OBOParseException("Tried to link to object "
						+ rs.getParent() + " that does not "
						+ "support linking.", rs.getPath(), rs.getLine(), rs
						.getLineNum());

			if (type == null) {
				if (allowDanglingParents) {
					type = (OBOProperty) objectFactory.createDanglingObject(rs
							.getType(), true);
				} else {
					throw new OBOParseException("Unrecognized type "
							+ rs.getType(), rs.getPath(), rs.getLine(), rs
							.getLineNum());
				}
			} else if (!(type instanceof OBOProperty))
				throw new OBOParseException("Tried to use non-type "
						+ rs.getType() + " as relationship " + "type", rs
						.getPath(), rs.getLine(), rs.getLineNum());

			Namespace ns = null;

			if (rs.getNamespace() != null) {

				ns = (Namespace) namespaceMap.get(rs.getNamespace());
				System.err.println("read namespace " + rs.getNamespace()
						+ " for link, fetched namespace " + ns);
				if (ns == null) {
					ns = objectFactory.createNamespace(rs.getNamespace(), null);
					namespaceMap.put(rs.getNamespace(), ns);
					session.addNamespace(ns);
				}
			}

			OBORestriction tr = objectFactory.createOBORestriction(
					(LinkedObject) child, (OBOProperty) type,
					(LinkedObject) parent, rs.isImplied());
			if (tr.isImplied())
				System.err.println("loaded implied rel " + tr);
			tr.setNecessarilyTrue(rs.isNecessary());
			tr.setInverseNecessarilyTrue(rs.isInverseNecessary());
			tr.setNestedValue(rs.getNestedValue());
			tr.setCompletes(rs.completes());
			tr.setMaxCardinality(rs.getMaxCardinality());
			tr.setMinCardinality(rs.getMinCardinality());
			tr.setCardinality(rs.getCardinality());

			if (ns != null && !ns.equals(child.getNamespace())) {
				tr.setNamespace(ns);
			}
			tr.getParent().addChild(tr);
		}
		it = rangeMap.keySet().iterator();
		while (it.hasNext()) {
			if (halted)
				throw new OBOParseException("Operation cancelled " + "by user",
						null, null, -1);

			OBOProperty t = (OBOProperty) it.next();
			String rangeID = (String) rangeMap.get(t);
			IdentifiedObject o = getObject(rangeID);
			if (o == null) {
				if (allowDanglingParents) {
					DanglingObject dangling = objectFactory
							.createDanglingObject(rangeID, false);
					System.err.println("assigned DANGLING " + dangling
							+ " to property " + t.getID());
					t.setRange(dangling);
					System.err.println("      range = " + t.getRange());
				} else
					throw new OBOParseException("Assigned non-existant "
							+ "range id " + rangeID + " to term " + t.getID(),
							null, null, -1);
			} else if (!(o instanceof Type))
				throw new OBOParseException("Assigned non-type to range of "
						+ "term " + t.getID(), null, null, -1);
			else {
				t.setRange((Type) o);
			}
		}
		it = domainMap.keySet().iterator();
		while (it.hasNext()) {
			if (halted)
				throw new OBOParseException("Operation cancelled " + "by user",
						null, null, -1);

			OBOProperty t = (OBOProperty) it.next();
			String domainID = (String) domainMap.get(t);
			IdentifiableObject domain = getObject(domainID);
			if (domain == null) {
				if (allowDanglingParents) {
					DanglingObject dangling = objectFactory
							.createDanglingObject(domainID, false);
					t.setDomain(dangling);
				} else
					throw new OBOParseException("Assigned non-existant domain "
							+ "id " + domainID + " to term " + t.getID(), null,
							null, -1);

			} else {
				if (!(domain instanceof OBOClass))
					throw new OBOParseException("Cannot use non-term " + domain
							+ " as domain value.", null, null, -1);

				t.setDomain((OBOClass) domain);
			}
		}

		it = instanceOfHash.keySet().iterator();
		while (it.hasNext()) {
			if (halted)
				throw new OBOParseException("Operation cancelled " + "by user",
						null, null, -1);

			String id = (String) it.next();
			InstanceStruct is = (InstanceStruct) instanceOfHash.get(id);

			IdentifiableObject instance = getObject(id);
			if (instance == null)
				throw new OBOParseException("Unexpected condition, "
						+ "unrecognized instance " + id + " found", is
						.getPath(), is.getLine(), is.getLineNum());
			IdentifiableObject instanceOfObj = getObject(is.instanceOf);

			if (instanceOfObj == null) {
				if (allowDanglingParents) {
					instanceOfObj = objectFactory
					.createDanglingObject(is.instanceOf, false);
				}
				else {
					throw new OBOParseException("Unrecognized instance_of id "
							+ is.instanceOf + " specified for " + "instance id "
							+ id, is.getPath(), is.getLine(), is.getLineNum());
				}
			}
			else {
				if (!(instanceOfObj instanceof OBOClass))
					throw new OBOParseException("Cannot use non-term value "
							+ is.instanceOf + ", " + "for instance_of statement.",
							is.getPath(), is.getLine(), is.getLineNum());

				((Instance) instance).setType((OBOClass) instanceOfObj);
			}
		}

		it = propertyValSet.iterator();
		while (it.hasNext()) {
			if (halted)
				throw new OBOParseException("Operation cancelled " + "by user",
						null, null, -1);

			PropertyValStruct pvs = (PropertyValStruct) it.next();
			Instance instance = (Instance) getObject(pvs.instanceID);
			AnnotatedObject prop_o = (AnnotatedObject) session
					.getObject(pvs.propID);
			if (instance == null)
				throw new OBOParseException("Unexpected condition "
						+ "encountered. Missing " + "instance.", pvs.getPath(),
						pvs.getLine(), pvs.getLineNum());
			if (prop_o == null) {
				if (allowDanglingParents) {
					prop_o = (OBOProperty) objectFactory.createDanglingObject(
							pvs.propID, true);
				} else {
					throw new OBOParseException("Unrecognized property "
							+ pvs.propID, pvs.getPath(), pvs.getLine(), pvs
							.getLineNum());
				}
			}

			if (!(prop_o instanceof OBOProperty))
				throw new OBOParseException("Non-property " + pvs.propID
						+ " specified as" + "property.", pvs.getPath(), pvs
						.getLine(), pvs.getLineNum());

			OBOProperty prop = (OBOProperty) prop_o;

			IdentifiedObject type_o = session.getObject(pvs.typeID);

			if (pvs.quoted) {
				if (pvs.typeID == null)
					type_o = Datatype.STRING;

				if (type_o == null)
					throw new OBOParseException("Unrecognized datatype "
							+ pvs.typeID, pvs.getPath(), pvs.getLine(), pvs
							.getLineNum());

				if (!(type_o instanceof Datatype))
					throw new OBOParseException("Non-datatype " + pvs.typeID
							+ " specified " + "as datatype", pvs.getPath(), pvs
							.getLine(), pvs.getLineNum());

				Datatype type = (Datatype) type_o;
				if (!type.isLegalValue(pvs.val)) {
					throw new OBOParseException("Illegal value " + pvs.val
							+ "for type " + type, pvs.getPath(), pvs.getLine(),
							pvs.getLineNum());
				}
				if (prop.getRange() != null) {
					if (!(prop.getRange() instanceof Datatype))
						throw new OBOParseException("Datatype specified "
								+ "for property with " + "non-datatype range",
								pvs.getPath(), pvs.getLine(), pvs.getLineNum());
				}
				instance.addPropertyValue(prop, new DatatypeValueImpl(type,
						pvs.val));
			} else {
				IdentifiedObject o = session.getObject(pvs.val);
				if (o == null) {
					if (allowDanglingParents) {
						o = objectFactory.createDanglingObject(pvs.val, false);
						session.addObject(o);
						System.err.println("added dangling object " + o);
					} else {
						danglingViolations.add(pvs);
						continue;
					}
				}
				if (type_o != null) {
					throw new OBOParseException("Cannot assign a type to "
							+ "a non-datatype property " + "value.", pvs
							.getPath(), pvs.getLine(), pvs.getLineNum());
				}

				System.err.println("pvs.val = " + pvs.val + ", o = " + o);
				if (!(o instanceof Value)) {
					throw new OBOParseException("Attempted to assign "
							+ "non value to a " + "propertyValue", pvs
							.getPath(), pvs.getLine(), pvs.getLineNum());
				} else
					instance.addPropertyValue(prop, o);
			}
		}

		if (danglingViolations.size() > 0) {
			// Collections.sort(danglingViolations);
			String message = danglingViolations.size()
					+ " unrecognized parent terms:\n";
			int linenum = -1;
			String line = null;
			String path = null;
			String parent = null;
			for (int i = 0; i < 20 && i < danglingViolations.size(); i++) {
				Object o = danglingViolations.get(i);
				if (o instanceof RelStruct) {
					RelStruct rs = (RelStruct) o;
					linenum = rs.getLineNum();
					line = rs.getLine();
					path = rs.getPath();
					parent = rs.getParent();
				} else if (o instanceof PropertyValStruct) {
					PropertyValStruct pvs = (PropertyValStruct) o;
					linenum = pvs.getLineNum();
					line = pvs.getLine();
					path = pvs.getPath();
					parent = pvs.val;
				}
				message += "     line " + linenum + ": " + parent + " of "
						+ path + "\n";
			}
			/*
			 * new GOBOParseException("Unrecognized parent "+ rs.getParent(),
			 * rs.getLine(), rs.getLineNum());
			 */
			throw new OBOParseException(message, path, line, linenum);
		}

		it = useSet.iterator();
		while (it.hasNext()) {
			if (halted)
				throw new OBOParseException("Operation cancelled " + "by user",
						null, null, -1);

			BasicMapping bm = (BasicMapping) it.next();
			IdentifiedObject subject = session.getObject(bm.getSubject());

			if (subject == null) {
				throw new OBOParseException(
						"Unexpected condition: subject " + "of replaced_by "
								+ bm.getSubject() + " disappeared!", bm
								.getPath(), bm.getLine(), bm.getLineNum());
			}

			if (!(subject instanceof ObsoletableObject)) {
				throw new OBOParseException("Attempted to assign "
						+ "replaced_by " + "value to non-obsoletable "
						+ "object " + subject, bm.getPath(), bm.getLine(), bm
						.getLineNum());
			}

			IdentifiedObject object = session.getObject(bm.getObject());

			if (object == null) {
				if (allowDanglingParents) {
					object = new DanglingClassImpl(bm.getObject());
				} else
					throw new OBOParseException("Could not resolve id "
							+ bm.getObject() + " in replaced_by "
							+ "statement.", bm.getPath(), bm.getLine(), bm
							.getLineNum());
			}

			if (!(object instanceof ObsoletableObject)) {
				throw new OBOParseException("replaced_by tag has "
						+ "non-obsoletable value " + object, bm.getPath(), bm
						.getLine(), bm.getLineNum());
			}
			/*
			 * ((ObsoletableObject) subject).
			 * addConsiderRelacement((ObsoletableObject) object); if
			 * (bm.getNestedValue() != null) { ((ObsoletableObject) subject).
			 * addConsiderExtension((ObsoletableObject) object,
			 * bm.getNestedValue()); }
			 */
			/*
			 * if (((ObsoletableObject) subject).getReplacedBy() != null) throw
			 * new GOBOParseException("Attempted to assign multiple "+
			 * "replaced_by values to "+ subject.getID(), bm.getPath(),
			 * bm.getLine(), bm.getLineNum());
			 */
			if (!((ObsoletableObject) subject).isObsolete())
				throw new OBOParseException("Attempted to specify "
						+ "replaced_by value for " + "non-obsolete object "
						+ subject + ".", bm.getPath(), bm.getLine(), bm
						.getLineNum());

			if (((ObsoletableObject) object).isObsolete() && failFast)
				throw new OBOParseException("Attempted to specify "
						+ "obsolete value " + object + " for "
						+ "replaced_by tag.", bm.getPath(), bm.getLine(), bm
						.getLineNum());

			((ObsoletableObject) subject)
					.addReplacedBy((ObsoletableObject) object);
			if (bm.getNestedValue() != null) {
				((ObsoletableObject) subject).addReplacedByExtension(
						(ObsoletableObject) object, bm.getNestedValue());
			}
		}

		it = considerSet.iterator();
		while (it.hasNext()) {
			if (halted)
				throw new OBOParseException("Operation cancelled " + "by user",
						null, null, -1);

			BasicMapping bm = (BasicMapping) it.next();
			IdentifiedObject subject = session.getObject(bm.getSubject());

			if (subject == null) {
				throw new OBOParseException("Unexpected condition: subject "
						+ "of consider " + bm.getSubject() + " disappeared!",
						bm.getPath(), bm.getLine(), bm.getLineNum());
			}

			if (!(subject instanceof ObsoletableObject)) {
				throw new OBOParseException("Attempted to assign "
						+ "consider " + "value to non-obsoletable " + "object "
						+ subject, bm.getPath(), bm.getLine(), bm.getLineNum());
			}

			IdentifiedObject object = session.getObject(bm.getObject());

			if (object == null) {
				if (allowDanglingParents)
					object = new DanglingClassImpl(bm.getObject());
				else
					throw new OBOParseException("Could not resolve id "
							+ bm.getObject() + " in consider " + "tag.", bm
							.getPath(), bm.getLine(), bm.getLineNum());
			}

			if (!(object instanceof ObsoletableObject)) {
				throw new OBOParseException("consider tag has "
						+ "non-obsoletable value " + object, bm.getPath(), bm
						.getLine(), bm.getLineNum());
			}

			if (!((ObsoletableObject) subject).isObsolete())
				throw new OBOParseException("Attempted to specify "
						+ "consider value for " + "non-obsolete object "
						+ subject + ".", bm.getPath(), bm.getLine(), bm
						.getLineNum());

			if (((ObsoletableObject) object).isObsolete())
				throw new OBOParseException("Attempted to specify "
						+ "obsolete value " + object + " for "
						+ "consider tag.", bm.getPath(), bm.getLine(), bm
						.getLineNum());

			((ObsoletableObject) subject)
					.addConsiderReplacement((ObsoletableObject) object);
			if (bm.getNestedValue() != null) {
				((ObsoletableObject) subject).addConsiderExtension(
						(ObsoletableObject) object, bm.getNestedValue());
			}
		}

		for (int i = 0; i < unknownStanzaList.size(); i++) {
			UnknownStanza us = (UnknownStanza) unknownStanzaList.get(i);
			session.addUnknownStanza(us);
		}

		session.setIDProfile(currentProfile);

		linkSet.clear();
		namespaceMap.clear();

		pathSet.clear();

		for (ParserExtension extension : parserExtensions) {
			extension.endParse();
		}
	}

	public void startStanza(String name) throws OBOParseException {
		if (currentObject != null && currentObject.getNamespace() == null
				&& getDefaultNamespace() != null)
			currentObject.setNamespace(getDefaultNamespace());
		currentStanza = name;
		if (!currentStanza.equalsIgnoreCase("term")
				&& !currentStanza.equalsIgnoreCase("typedef")
				&& !currentStanza.equalsIgnoreCase("instance")) {
			unknownStanza = new UnknownStanza(currentStanza,
					getDefaultNamespace());
			unknownStanzaList.add(unknownStanza);
		} else
			unknownStanza = null;
		for (ParserExtension extension : parserExtensions) {
			extension.startStanza(name);
		}
	}

	public void readBangComment(String comment) throws OBOParseException {
		for (ParserExtension extension : parserExtensions) {
			extension.readBangComment(comment);
		}
	}

	public void readTagValue(String name, String value, NestedValue nv,
			boolean handled) throws OBOParseException {
		if (!handled) {
			PropertyValue pv = new PropertyValueImpl(name, value);
			if (currentStanza == null)
				session.addPropertyValue(pv);
			else if (unknownStanza != null)
				unknownStanza.addPropertyValue(pv);
			else
				currentObject.addPropertyValue(pv);
		}
		for (ParserExtension extension : parserExtensions) {
			extension.readTagValue(name, value, nv, handled);
		}
	}

	public void setParseEngine(ParseEngine engine) {
		this.engine = engine;
		for (ParserExtension extension : parserExtensions) {
			extension.setParseEngine(engine);
		}
	}

	public OBOMetaData getMetaData() {
		return metaData;
	}

	public void setFailFast(boolean failFast) {
		this.failFast = failFast;
	}

	public void readCreatedBy(String user, NestedValue val)
			throws OBOParseException {
		if (!(currentObject instanceof ModificationMetadataObject))
			throw new OBOParseException("Attempted to set created-by field of "
					+ "object " + currentObject
					+ " which does not support modification metadata.",
					getCurrentPath(), engine.getCurrentLine(), engine
							.getLineNum());
		((ModificationMetadataObject) currentObject).setCreatedBy(user);
		((ModificationMetadataObject) currentObject).setCreatedByExtension(val);
	}

	public void readCreationDate(Date date, NestedValue val)
			throws OBOParseException {
		if (!(currentObject instanceof ModificationMetadataObject))
			throw new OBOParseException(
					"Attempted to set creation-date field of " + "object "
							+ currentObject
							+ " which does not support modification metadata.",
					getCurrentPath(), engine.getCurrentLine(), engine
							.getLineNum());
		((ModificationMetadataObject) currentObject).setCreationDate(date);
		((ModificationMetadataObject) currentObject)
				.setCreationDateExtension(val);

	}

	public void readModificationDate(Date date, NestedValue val)
			throws OBOParseException {
		if (!(currentObject instanceof ModificationMetadataObject))
			throw new OBOParseException(
					"Attempted to set modification-date field of " + "object "
							+ currentObject
							+ " which does not support modification metadata.",
					getCurrentPath(), engine.getCurrentLine(), engine
							.getLineNum());
		((ModificationMetadataObject) currentObject).setModificationDate(date);
		((ModificationMetadataObject) currentObject)
				.setModificationDateExtension(val);
	}

	public void readModifiedBy(String user, NestedValue val)
			throws OBOParseException {
		if (!(currentObject instanceof ModificationMetadataObject))
			throw new OBOParseException(
					"Attempted to set modified-by field of " + "object "
							+ currentObject
							+ " which does not support modification metadata.",
					getCurrentPath(), engine.getCurrentLine(), engine
							.getLineNum());
		((ModificationMetadataObject) currentObject).setModifiedBy(user);
		((ModificationMetadataObject) currentObject)
				.setModifiedByExtension(val);
	}

	public void readImpliedID() {
		readID(IDUtil.fetchTemporaryID(session), new NestedValueImpl());
	}
}
