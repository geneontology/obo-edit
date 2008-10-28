package org.obo.dataadapter;

import org.bbop.dataadapter.*;
import org.bbop.expression.JexlContext;
import org.bbop.io.SafeFileOutputStream;
import org.bbop.util.*;
import org.obo.annotation.dataadapter.AnnotationParserExtension;
import org.obo.dataadapter.OBOConstants.StanzaMapping;
import org.obo.dataadapter.OBOConstants.TagMapping;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.obo.filters.*;
import org.obo.identifier.DefaultIDGenerator;
import org.obo.identifier.IDProfile;
import org.obo.identifier.IDRule;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerFactory;
import org.obo.reasoner.impl.ForwardChainingReasoner;
import org.obo.reasoner.impl.ForwardChainingReasonerFactory;
import org.obo.reasoner.impl.LinkPileReasonerFactory;
import org.obo.reasoner.impl.TrimmedLinkDatabase;
import org.obo.util.ReasonerUtil;
import org.obo.util.TermUtil;

import java.io.*;
import java.util.*;

import org.apache.log4j.*;

/**
 * Writes OBO Files.
 * Delegates work to OBOSerializer
 *
 */
public class OBOSerializationEngine extends AbstractProgressValued {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OBOSerializationEngine.class);

	protected static int DONT_WRITE_ID_RULES = 0;

	protected static int WRITE_STORED_ID_RULES = 1;

	protected static int WRITE_CURRENT_ID_RULES = 2;

	public static final String SAVE_FOR_PRESENTATION = "Save for presentation";

	public static final String SAVE_ALL = "Save all links";

	public static class FilteredPath {

		protected Filter linkFilter;

		protected Filter objectFilter;

		protected String prefilterProperty;

		protected boolean doFilter = false;

		protected boolean doLinkFilter = false;

	    protected boolean allowDangling = true;  // Default is now true!

		protected boolean saveImplied = false;
		

		// FCR does not appear to work with regulation examples...
		protected ReasonerFactory reasonerFactory = new
		  LinkPileReasonerFactory();
//		protected ReasonerFactory reasonerFactory = new ForwardChainingReasonerFactory();

		protected boolean discardUnusedCategories = false;

		protected boolean useSessionReasoner = true;

		protected int idRuleMode = DONT_WRITE_ID_RULES;

		protected String impliedType = SAVE_FOR_PRESENTATION;

		protected String path;

		protected String remark;

		protected String rootAlgorithm = "GREEDY";

		protected boolean realizeImpliedLinks = false;

		protected boolean saveTypes;

		protected boolean writeModificationData = false;

		public FilteredPath() {
		}

		public FilteredPath(Filter linkFilter, Filter objectFilter, String path) {
			setLinkFilter(linkFilter);
			setObjectFilter(objectFilter);
			setPath(path);
		}

		public boolean getUseSessionReasoner() {
			return useSessionReasoner;
		}

		public void setUseSessionReasoner(boolean useSessionReasoner) {
			this.useSessionReasoner = useSessionReasoner;
		}

		public void setRootAlgorithm(String rootAlgorithm) {
			this.rootAlgorithm = rootAlgorithm;
		}

		public String getRootAlgorithm() {
			return rootAlgorithm;
		}

		public void setPrefilterProperty(String prefilterProperty) {
			this.prefilterProperty = prefilterProperty;
		}

		public String getPrefilterProperty() {
			return prefilterProperty;
		}

		public void setRemark(String remark) {
			this.remark = remark;
		}

		public String getRemark() {
			return remark;
		}

		public void setIDRuleMode(int idRuleWriteMode) {
			this.idRuleMode = idRuleWriteMode;
		}

		public int getIDRuleMode() {
			return idRuleMode;
		}

		public void setAllowDangling(boolean allowDangling) {
			this.allowDangling = allowDangling;
		}

		public boolean getAllowDangling() {
			return allowDangling;
		}

		public void setSaveImplied(boolean saveImplied) {
			this.saveImplied = saveImplied;
		}

		public boolean getSaveImplied() {
			return saveImplied;
		}
		
		public void setPath(String path) {
			this.path = path;
		}

		public void setImpliedType(String impliedType) {
			this.impliedType = impliedType;
		}

		public String getImpliedType() {
			return impliedType;
		}

		public String getPath() {
			return path;
		}

		public boolean getDoLinkFilter() {
			return doLinkFilter;
		}

		public void setDoLinkFilter(boolean doLinkFilter) {
			this.doLinkFilter = doLinkFilter;
		}

		public boolean getDoFilter() {
			return doFilter;
		}

		public void setDoFilter(boolean doFilter) {
			this.doFilter = doFilter;
		}

		@Override
		public String toString() {
			return path;
		}

		public boolean getRealizeImpliedLinks() {
			return realizeImpliedLinks;
		}

		public void setRealizeImpliedLinks(boolean realizeImpliedLinks) {
			this.realizeImpliedLinks = realizeImpliedLinks;
		}

		public boolean getSaveTypes() {
			return saveTypes;
		}

		public void setSaveTypes(boolean saveTypes) {
			this.saveTypes = saveTypes;
		}

		public boolean getDiscardUnusedCategories() {
			return discardUnusedCategories;
		}

		public void setDiscardUnusedCategories(boolean discardUnusedCategories) {
			this.discardUnusedCategories = discardUnusedCategories;
		}

		public Filter getLinkFilter() {
			return linkFilter;
		}

		public void setLinkFilter(Filter linkFilter) {
			this.linkFilter = linkFilter;
		}

		public Filter getObjectFilter() {
			return objectFilter;
		}

		public void setObjectFilter(Filter objectFilter) {
			this.objectFilter = objectFilter;
		}

		public ReasonerFactory getReasonerFactory() {
			return reasonerFactory;
		}

		public void setReasonerFactory(ReasonerFactory reasonerFactory) {
			this.reasonerFactory = reasonerFactory;
		}

		public boolean getWriteModificationData() {
			return writeModificationData;
		}

		public void setWriteModificationData(boolean writeModificationData) {
			this.writeModificationData = writeModificationData;
		}
	}

	protected boolean cancelled = false;

	protected List streams = new LinkedList();

	protected List closingStreams = new LinkedList();

	protected List progressListeners = new LinkedList();

	protected List<OBOSerializerExtension> extensions = new LinkedList<OBOSerializerExtension>();

	protected OBOSerializer serializer;

	protected boolean allowDangling;

	protected boolean saveImplied;

	protected ReasonerFactory reasonerFactory;

	protected boolean realizeImplied;

	protected List<TagMapping> tagOrdering;

	protected List<StanzaMapping> stanzaOrdering;

	protected List<TagMapping> headerTagOrdering;

	protected ReasonedLinkDatabase reasoner;

	protected IDProfile currentProfile;
	protected String autogenString;
	protected String username;

	protected boolean writeModificationData;

	protected static final char[] generic_escapes = { '{', '!', '\n', '\r' };

	protected static final char[] blocktext_escapes = { '{', '\n', '\t', '!' };

	protected static final char[] space_escapes = { '{', '"', ' ', '\t', '!' };

	protected static final char[] single_quote_escapes = { '\'', '\n' };

	protected static final char[] double_quote_escapes = { '"', '\n', '\r' };

	protected static final char[] before_quote_escapes = { '"', '{', '!' };

	protected static final char[] pv_name_escapes = { '=', ' ', '\t', '!' };

	protected static final char[] pv_value_escapes = { ',', '}', ' ', '\t', '!' };

	protected static final char[] dbxref_escapes = { '{', ':', '\t', ',', '"',
			']', '!' };

	public static String escapeSpaces(String s) {
		return escape(s, space_escapes);
	}

	public static String escapeQuoted(String s, char quoteChar) {
		char[] escapes;
		if (quoteChar == '\'')
			escapes = single_quote_escapes;
		else if (quoteChar == '\"')
			escapes = double_quote_escapes;
		else {
			escapes = new char[2];
			escapes[0] = quoteChar;
			escapes[1] = '\n';
		}
		return escape(s, escapes);
	}

	public static String escapeBeforeQuotes(String s) {
		return escape(s, before_quote_escapes);
	}

	public static String escapePVName(String s) {
		return escape(s, pv_name_escapes);
	}

	public static String escapePVValue(String s) {
		return escape(s, pv_value_escapes);
	}

	public static String escapeDbxref(String s) {
		return escape(s, dbxref_escapes);
	}

	public static String escapeBlocktext(String s) {
		return escape(s, blocktext_escapes);
	}

	public static String escape(String s) {
		return escape(s, generic_escapes);
	}

	public static String escape(String s, char[] charsToEscape) {
		StringBuffer out = new StringBuffer();
		for (int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			boolean found = false;
			if (c == '\\') {
				found = true;
			} else {
				for (int j = 0; j < charsToEscape.length; j++) {
					if (charsToEscape[j] == c) {
						found = true;
						break;
					}
				}
			}
			if (found) {
				out.append('\\');
				if (c == '\n')
					out.append('n');
				else if (c == '\r') // CJM - normalize newlines
					out.append('n');
				else if (c == '\\')
					out.append('\\');
				else if (c == ' ')
					out.append('W');
				else
					out.append(c);
			} else
				out.append(c);
		}
		return out.toString();
	}

	public OBOSerializationEngine() {
		try {
			addSerializerExtension(new PostcompParserExtension());
			addSerializerExtension(new AnnotationParserExtension());
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public void cancel() {
		cancelled = true;
		Iterator it = streams.iterator();
		while (it.hasNext()) {
			SafeFileOutputStream sfos = (SafeFileOutputStream) it.next();
			try {
				sfos.fail();
			} catch (IOException ex) {
				// do nothing
			}
		}
	}

	public void serialize(OBOSession session, OBOSerializer serializer,
			String path) throws DataAdapterException {
		serialize(session, serializer, Collections.singleton(new FilteredPath(
				null, null, path)));
	}

	public void serialize(OBOSession session, OBOSerializer serializer,
			Collection<FilteredPath> filteredPaths) throws DataAdapterException {
		setSerializer(serializer);
		streams.clear();
		cancelled = false;
		Iterator<FilteredPath> it = filteredPaths.iterator();

		while (it.hasNext()) {
			FilteredPath filteredPath = it.next();
			try {
				// First make sure we can actually write to this path
				// (SafeFileOutputStream won't tell us, because it first writes to a temp file)
				try {
					FileOutputStream fos = new FileOutputStream(filteredPath.getPath());
					fos.close();
				}
				catch (Exception e) {
					logger.warn("Can't write to output file " + filteredPath.getPath());
					throw new DataAdapterException("Can't write to output file " + filteredPath.getPath());
				}
				SafeFileOutputStream sfos = new SafeFileOutputStream(
						filteredPath.getPath());
				streams.add(sfos);
				PrintStream stream = new PrintStream(new BufferedOutputStream(
						sfos));
				closingStreams.add(stream);

				Filter linkFilter = filteredPath.getLinkFilter();
				Filter objectFilter = filteredPath.getObjectFilter();
				if (!filteredPath.getDoFilter())
					objectFilter = null;
				else if (filteredPath.getSaveTypes()) {
					CompoundFilter cf = new CompoundFilterImpl();

					ObjectFilter propCriterion = new ObjectFilterImpl();
					propCriterion.setNegate(false);
					propCriterion.setCriterion(new IsPropertyCriterion());

					cf.setBooleanOperation(CompoundFilter.OR);
					cf.addFilter(objectFilter);
					cf.addFilter(propCriterion);
					objectFilter = cf;
				}
				if (!filteredPath.getDoLinkFilter())
					linkFilter = null;
				writeFile(session, objectFilter, linkFilter, serializer,
						stream, filteredPath);
			} catch (IOException ex) {
				throw new DataAdapterException("Write error", ex);
			}
		}
		Iterator it2 = closingStreams.iterator();
		while (it2.hasNext()) {
			OutputStream os = (OutputStream) it2.next();
			try {
				os.close();
			} catch (IOException ex) {
				throw new DataAdapterException("Could not commit changes to "
						+ os);
			}
		}
		closingStreams.clear();
		streams.clear();
	}

	protected void setSerializer(OBOSerializer serializer) {
		this.serializer = serializer;
		serializer.setEngine(this);
	}

	public void writeHeader(OBOSession session, LinkDatabase linkDatabase,
			OBOSerializer serializer, FilteredPath filteredPath)
			throws IOException, CancelledAdapterException {
		serializer.startHeader();
		String remark = filteredPath.getRemark();
		if (remark == null) {
			remark = session.getCurrentHistory().getComment();
		}
		List scratchList = new ArrayList();
		List headerTagOrdering = serializer.getHeaderTagOrdering();
		if (headerTagOrdering == null)
			headerTagOrdering = OBOConstants.DEFAULT_HEADER_TAG_ORDER;

		Iterator it = headerTagOrdering.iterator();
		while (it.hasNext()) {
			Object tagSpec = it.next();
			if (tagSpec.equals(OBOConstants.FORMAT_VERSION_HEADER_TAG)) {
				serializer.writeFormatVersionHeaderTag();
			} else if (tagSpec.equals(OBOConstants.DATA_VERSION_HEADER_TAG)) {
				if (session.getCurrentHistory().getVersion() != null)
					serializer.writeDataVersionHeaderTag(session
							.getCurrentHistory().getVersion());
			} else if (tagSpec.equals(OBOConstants.DATE_HEADER_TAG)) {
				Date date = (new GregorianCalendar(TimeZone.getTimeZone("GMT")))
						.getTime();
				serializer.writeDateHeaderTag(date);
			} else if (tagSpec.equals(OBOConstants.SAVED_BY_HEADER_TAG)) {
				String username = getUsername();
				if (username != null)
					serializer.writeSavedByHeaderTag(username);
			} else if (tagSpec
					.equals(OBOConstants.AUTO_GENERATED_BY_HEADER_TAG)) {
				if (getAutogenString() != null)
					serializer
							.writeAutoGeneratedByHeaderTag(getAutogenString());
			} else if (tagSpec.equals(OBOConstants.IDSPACE_HEADER_TAG)) {
				for (String idspace : session.getIDSpaces())
					serializer.writeIDSpaceHeaderTag(idspace, session
							.expandIDSpace(idspace));
			} else if (tagSpec.equals(OBOConstants.SUBSETDEF_HEADER_TAG)) {
				Comparator comparator = serializer.getCategoryComparator();
				if (comparator == null)
					comparator = OBOConstants.DEFAULT_CATEGORY_COMPARATOR;
				scratchList.clear();
				if (filteredPath.getDiscardUnusedCategories()) {
					Set usedCategories = new HashSet();
					Iterator it2 = linkDatabase.getObjects().iterator();
					while (it2.hasNext()) {
						IdentifiedObject io = (IdentifiedObject) it2.next();
						if (io instanceof CategorizedObject) {
							CategorizedObject co = (CategorizedObject) io;
							usedCategories.addAll(co.getCategories());
						}
					}
					scratchList.addAll(usedCategories);
				} else
					scratchList.addAll(session.getCategories());
				Collections.sort(scratchList, comparator);
				Iterator it2 = scratchList.iterator();
				while (it2.hasNext()) {
					Object o = it2.next();
					TermCategory cat = (TermCategory) o;
					serializer.writeSubsetDefHeaderTag(cat);
				}
			} else if (tagSpec
					.equals(OBOConstants.RELATED_SYNONYMTYPEDEF_HEADER_TAG)) {
				Comparator comparator = serializer
						.getSynonymCategoryComparator();
				if (comparator == null)
					comparator = OBOConstants.DEFAULT_RELATED_SYNONYM_CATEGORY_COMPARATOR;
				scratchList.clear();
				if (filteredPath.getDiscardUnusedCategories()) {
					Set usedCategories = new HashSet();
					Iterator it2 = linkDatabase.getObjects().iterator();
					while (it2.hasNext()) {
						IdentifiedObject io = (IdentifiedObject) it2.next();
						if (io instanceof SynonymedObject) {
							SynonymedObject so = (SynonymedObject) io;
							Iterator it3 = so.getSynonyms().iterator();
							while (it3.hasNext()) {
								Synonym s = (Synonym) it3.next();
								if (s.getSynonymCategory() != null)
									usedCategories.add(s.getSynonymCategory());
							}
						}
					}
					scratchList.addAll(usedCategories);
				} else
					scratchList.addAll(session.getSynonymCategories());
				Collections.sort(scratchList, comparator);
				Iterator it2 = scratchList.iterator();
				while (it2.hasNext()) {
					SynonymCategory cat = (SynonymCategory) it2.next();
					serializer.writeSynonymTypeDefHeaderTag(cat);
				}
			} else if (tagSpec
					.equals(OBOConstants.DEFAULT_NAMESPACE_HEADER_TAG)) {
				serializer.writeDefaultNamespaceHeaderTag(session
						.getDefaultNamespace());
			} else if (tagSpec
					.equals(OBOConstants.NAMESPACE_ID_RULE_HEADER_TAG)) {
				if (filteredPath.getIDRuleMode() == WRITE_STORED_ID_RULES
						|| filteredPath.getIDRuleMode() == WRITE_CURRENT_ID_RULES) {
					IDProfile profile = null;
					if (filteredPath.getIDRuleMode() == WRITE_STORED_ID_RULES) {
						profile = session.getIDProfile();
					}

					if (profile == null) {
						profile = currentProfile;
					}
					if (profile != null) {
						serializer.writeNamespaceIDRuleHeaderTag(null, profile
								.getDefaultRule());
						Map nsMap = new HashMap();
						List nsList = new ArrayList();
						Iterator it2 = profile.getRules().iterator();
						while (it2.hasNext()) {
							IDRule rule = (IDRule) it2.next();
							Filter filter = rule.getFilter();
							if (rule.getFilter() instanceof CompoundFilter) {
								CompoundFilter cf = (CompoundFilter) rule
										.getFilter();
								if (cf.getFilters().size() == 1)
									filter = (Filter) cf.getFilters().get(0);
							}
							if (filter != null
									&& filter instanceof ObjectFilter) {
								ObjectFilter of = (ObjectFilter) filter;
								if (of.getCriterion() instanceof NamespaceSearchCriterion
										&& of.getAspect() instanceof SelfSearchAspect
										&& of.getComparison() instanceof EqualsComparison
										&& !of.getNegate()) {
									String ns = of.getValue();
									nsMap.put(ns, rule.getRule());
									nsList.add(ns);
								}
							}
						}
						Collections.sort(nsList);
						it2 = nsList.iterator();
						while (it2.hasNext()) {
							String ns = (String) it2.next();
							serializer.writeNamespaceIDRuleHeaderTag(ns,
									(String) nsMap.get(ns));
						}
					}
				}
			} else if (tagSpec.equals(OBOConstants.REMARK_HEADER_TAG)) {
				serializer.writeRemarkHeaderTag(remark);
			}
		}
		for (PropertyValue pv : session.getPropertyValues()) {
			serializer.writeGenericHeaderTag(pv.getProperty(),pv.getValue());
		}
		serializer.endHeader();
	}

	public static Comparator getIDComparator(OBOSerializer serializer) {
		Comparator idComparator = serializer.getIDComparator();
		if (idComparator == null)
			idComparator = OBOConstants.DEFAULT_ID_COMPARATOR;
		return idComparator;
	}

	public static Comparator getDbxrefComparator(OBOSerializer serializer) {
		Comparator dbxrefComparator = serializer.getDbxrefComparator();
		if (dbxrefComparator == null)
			dbxrefComparator = OBOConstants.DEFAULT_DBXREF_COMPARATOR;
		return dbxrefComparator;
	}

	public static Comparator getCategoryComparator(OBOSerializer serializer) {
		Comparator categoryComparator = serializer.getCategoryComparator();
		if (categoryComparator == null)
			categoryComparator = OBOConstants.DEFAULT_CATEGORY_COMPARATOR;
		return categoryComparator;
	}

	public static Comparator getSynonymComparator(OBOSerializer serializer) {
		Comparator synonymComparator = serializer.getSynonymComparator();
		if (synonymComparator == null)
			synonymComparator = OBOConstants.DEFAULT_RELATED_SYNONYM_COMPARATOR;
		return synonymComparator;
	}

	public static Comparator getObsoleteComparator(OBOSerializer serializer) {
		Comparator obsoleteComparator = serializer.getObsoleteComparator();
		if (obsoleteComparator == null)
			obsoleteComparator = OBOConstants.DEFAULT_OBSOLETE_COMPARATOR;
		return obsoleteComparator;
	}

	public static Comparator getLinkComparator(OBOSerializer serializer) {
		Comparator linkComparator = serializer.getLinkComparator();
		if (linkComparator == null)
			linkComparator = OBOConstants.DEFAULT_LINK_COMPARATOR;
		return linkComparator;
	}

	public void writeObject(IdentifiedObject obj, LinkDatabase linkDatabase,
			OBOSerializer serializer) throws IOException,
			CancelledAdapterException {
		/*
		 * boolean writeNonRels = linkDatabase.getObjects().contains(obj);
		 * 
		 * 
		 * if (obj instanceof LinkedObject) { LinkedObject lo = (LinkedObject)
		 * obj;
		 * 
		 * scratchList.clear(); scratchList.addAll(linkDatabase.getParents(lo));
		 * Collections.sort(scratchList, linkComparator);
		 * linkList.addAll(scratchList); }
		 * 
		 * if (!writeNonRels && linkList.size() == 0) {
		 * TermUtil.freeList(linkList); return; }
		 */
		boolean written = false;
		for (OBOSerializerExtension extension : extensions) {
			if (extension.startStanza(obj)) {
				written = true;
				break;
			}
		}
		if (!written) {
		    // Don't write out bogus objects (which would come out as "[null]" stanzas).
		    // These bogus objects are created from dangling identifiers.
		    if (isRealObject(obj))
			serializer.startStanza(obj);
		    else {
			logger.info("OBOSerializationEngine.writeObject: not writing bogus object " + obj); // DEL
			return;
		    }
		}

		Iterator it = tagOrdering.iterator();
		while (it.hasNext()) {
			OBOConstants.TagMapping tagMapping = (OBOConstants.TagMapping) it
					.next();
			writeTag(tagMapping, obj, linkDatabase, serializer);
		}

		written = false;
		for (OBOSerializerExtension extension : extensions) {
			if (extension.endStanza(obj)) {
				written = true;
				break;
			}
		}
		if (!written)
			serializer.endStanza(obj);
	}

    /** Don't write out bogus objects (which would come out as "[null]" stanzas).
	(Is this the right test?) */
    protected boolean isRealObject(IdentifiedObject obj) {
	if (TermUtil.isClass(obj) ||
	    TermUtil.isProperty(obj) ||
	    TermUtil.isInstance(obj))
	    return true;
	else
	    return false;
    }

	protected void writeTag(TagMapping tagMapping, IdentifiedObject obj,
			LinkDatabase linkDatabase, OBOSerializer serializer)
			throws IOException {
		writeTag(tagMapping, obj, linkDatabase, serializer, allowDangling,
				realizeImplied, writeModificationData);
	}

	protected void writeTag(TagMapping tagMapping, IdentifiedObject obj,
			LinkDatabase linkDatabase, OBOSerializer serializer,
			boolean allowDangling, boolean realizeImpliedLinks,
			boolean writeModificationData) throws IOException {
		for (OBOSerializerExtension extension : extensions) {
			if (extension.writeTag(tagMapping, obj, linkDatabase))
				return;
		}
		if (tagMapping.equals(OBOConstants.ID_TAG)) {
			serializer.writeIDTag(obj.getID(), obj.getIDExtension());
		} else if (tagMapping.equals(OBOConstants.IS_ANONYMOUS_TAG)) {
			serializer.writeIsAnonymousTag(obj.isAnonymous(), obj
					.getAnonymousExtension());
		}

		if (tagMapping.equals(OBOConstants.NAME_TAG)) {
			serializer.writeNameTag(obj.getName(), obj.getNameExtension());
		} else if (tagMapping.equals(OBOConstants.NAMESPACE_TAG)) {
			serializer.writeNamespaceTag(obj.getNamespace(), obj
					.getNamespaceExtension());
		} else if (obj instanceof MultiIDObject
				&& tagMapping.equals(OBOConstants.ALT_ID_TAG)) {
			List scratchList = new LinkedList();
			scratchList.addAll(((MultiIDObject) obj).getSecondaryIDs());
			Collections.sort(scratchList, getIDComparator(serializer));
			Iterator it2 = scratchList.iterator();
			while (it2.hasNext()) {
				String id = (String) it2.next();
				NestedValue nv = ((MultiIDObject) obj)
				.getSecondaryIDExtension(id);
				serializer.writeAltIDTag(id, nv);
			}
		} else if (obj instanceof DefinedObject
				&& tagMapping.equals(OBOConstants.DEF_TAG)) {
			List scratchList = new LinkedList();
			scratchList.addAll(((DefinedObject) obj).getDefDbxrefs());
			Collections.sort(scratchList, getDbxrefComparator(serializer));
			serializer
			.writeDefTag(((DefinedObject) obj).getDefinition(),
					scratchList, ((DefinedObject) obj)
					.getDefinitionExtension());
		} else if (obj instanceof CommentedObject
				&& tagMapping.equals(OBOConstants.COMMENT_TAG)) {
			CommentedObject cobj = (CommentedObject) obj;
			if (cobj.getComment() != null)
				serializer.writeCommentTag(cobj.getComment(), cobj
						.getCommentExtension());
		} else if (obj instanceof ModificationMetadataObject
				&& tagMapping.equals(OBOConstants.CREATED_BY_TAG)) {
			ModificationMetadataObject mobj = (ModificationMetadataObject) obj;
			if (mobj.getCreatedBy() != null)
				serializer.writeCreatedByTag(mobj.getCreatedBy(), mobj
						.getCreatedByExtension());
		} else if (obj instanceof ModificationMetadataObject
				&& tagMapping.equals(OBOConstants.CREATION_DATE_TAG)) {
			ModificationMetadataObject mobj = (ModificationMetadataObject) obj;
			if (mobj.getCreationDate() != null)
				serializer.writeCreationDateTag(mobj.getCreationDate(), mobj
						.getCreationDateExtension());
		} else if (obj instanceof ModificationMetadataObject
				&& tagMapping.equals(OBOConstants.MODIFICATION_DATE_TAG)
				&& writeModificationData) {
			ModificationMetadataObject mobj = (ModificationMetadataObject) obj;
			if (mobj.getModificationDate() != null)
				serializer.writeModificationDateTag(mobj.getModificationDate(),
						mobj.getModificationDateExtension());
		} else if (obj instanceof ModificationMetadataObject
				&& tagMapping.equals(OBOConstants.MODIFIED_BY_TAG)
				&& writeModificationData) {
			ModificationMetadataObject mobj = (ModificationMetadataObject) obj;
			if (mobj.getModifiedBy() != null)
				serializer.writeModifiedByTag(mobj.getModifiedBy(), mobj
						.getModifiedByExtension());
		} else if (obj instanceof CategorizedObject
				&& tagMapping.equals(OBOConstants.SUBSET_TAG)) {

			List scratchList = new LinkedList();
			scratchList.addAll(((CategorizedObject) obj).getCategories());
			Collections.sort(scratchList, getCategoryComparator(serializer));
			Iterator it2 = scratchList.iterator();
			while (it2.hasNext()) {
				TermCategory category = (TermCategory) it2.next();
				NestedValue nv = ((CategorizedObject) obj)
				.getCategoryExtension(category);
				serializer.writeSubsetTag(category, nv);
			}
		} else if (obj instanceof SynonymedObject
				&& tagMapping.equals(OBOConstants.RELATED_SYNONYM_TAG)) {

			List scratchList = new LinkedList();
			scratchList.addAll(((SynonymedObject) obj).getSynonyms());
			Collections.sort(scratchList, getSynonymComparator(serializer));
			Iterator it2 = scratchList.iterator();
			while (it2.hasNext()) {
				Synonym synonym = (Synonym) it2.next();
				serializer.writeSynonymTag(synonym, synonym.getNestedValue());
			}
		} else if (obj instanceof DbxrefedObject
				&& tagMapping.equals(OBOConstants.XREF_TAG)) {
			List scratchList = new LinkedList();
			scratchList.addAll(((DbxrefedObject) obj).getDbxrefs());
			Collections.sort(scratchList, getDbxrefComparator(serializer));
			Iterator it2 = scratchList.iterator();
			while (it2.hasNext()) {
				Dbxref dbxref = (Dbxref) it2.next();
				serializer.writeXrefTag(dbxref);
			}
		} else if (obj instanceof Instance
				&& tagMapping.equals(OBOConstants.INSTANCE_OF_TAG)) {
			serializer.writeInstanceOfTag(((Instance) obj).getType(), null);
		} else if (obj instanceof Instance
				&& tagMapping.equals(OBOConstants.PROPERTY_VALUE_TAG)) {
			// not yet implemented
		} else if (obj instanceof Instance
				&& tagMapping.equals(OBOConstants.VALUE_LINK_TAG)) {
			if (obj instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject) obj;
				List linkList = new LinkedList();
				linkList.addAll(linkDatabase.getParents(lo));
				Collections.sort(linkList, getLinkComparator(serializer));
				Iterator it2 = linkList.iterator();
				while (it2.hasNext()) {
					Link link = (Link) it2.next();
					if (link instanceof ValueLink)
						serializer.writeValueLinkTag((ValueLink) link, link
								.getNestedValue());
				}
			}
		} else if (obj instanceof OBOProperty
				&& tagMapping.equals(OBOConstants.DOMAIN_TAG)) {
			OBOProperty property = (OBOProperty) obj;
			if (property.getDomain() != null
					&& (allowDangling || linkDatabase.getObject(property
							.getDomain().getID()) != null))
				serializer.writeDomainTag(property.getDomain(), property
						.getDomainExtension());
		} else if (obj instanceof OBOProperty
				&& tagMapping.equals(OBOConstants.RANGE_TAG)) {
			OBOProperty property = (OBOProperty) obj;
			if (property.getRange() != null
					&& (allowDangling || linkDatabase.getObject(property
							.getRange().getID()) != null))
				serializer.writeRangeTag(property.getRange(), property
						.getRangeExtension());
		} else if (obj instanceof OBOProperty
				&& tagMapping.equals(OBOConstants.IS_CYCLIC_TAG)) {
			OBOProperty property = (OBOProperty) obj;
			serializer.writeIsCyclicTag(property.isCyclic(), property
					.getCyclicExtension());
		} else if (obj instanceof OBOProperty
				&& tagMapping.equals(OBOConstants.IS_REFLEXIVE_TAG)) {
			OBOProperty property = (OBOProperty) obj;
			serializer.writeIsReflexiveTag(property.isReflexive(), property
					.getReflexiveExtension());
		} else if (obj instanceof OBOProperty
				&& tagMapping.equals(OBOConstants.ALWAYS_IMPLIES_INVERSE_TAG)) {
			OBOProperty property = (OBOProperty) obj;
			serializer
			.writeAlwaysImpliesInverseTag(property
					.isAlwaysImpliesInverse(), property
					.getReflexiveExtension());
		} else if (obj instanceof OBOProperty
				&& tagMapping.equals(OBOConstants.IS_SYMMETRIC_TAG)) {
			OBOProperty property = (OBOProperty) obj;
			serializer.writeIsSymmetricTag(property.isSymmetric(), property
					.getSymmetricExtension());
		} else if (obj instanceof OBOProperty
				&& tagMapping.equals(OBOConstants.IS_TRANSITIVE_TAG)) {
			OBOProperty property = (OBOProperty) obj;
			serializer.writeIsTransitiveTag(property.isTransitive(), property
					.getTransitiveExtension());
		} else if (obj instanceof OBOProperty
				&& tagMapping.equals(OBOConstants.TRANSITIVE_OVER_TAG)) {
			OBOProperty property = (OBOProperty) obj;
			for (Link link : property.getParents()) {
				if (link.getType().equals(OBOProperty.TRANSITIVE_OVER)) {
					serializer.writeLinkTag(link, null);
				}
			}
		} else if (obj instanceof OBOProperty
				&& tagMapping.equals(OBOConstants.DISJOINT_OVER_TAG)) {
			OBOProperty property = (OBOProperty) obj;
			for (Link link : property.getParents()) {
				if (link.getType().equals(OBOProperty.DISJOINT_OVER)) {
					serializer.writeLinkTag(link, null);
				}
			}
		} else if (obj instanceof OBOProperty
				&& tagMapping.equals(OBOConstants.HOLDS_OVER_CHAIN_TAG)) {
			OBOProperty property = (OBOProperty) obj;
			if (property.getHoldsOverChains() != null) {
				for (List<OBOProperty> chain : property.getHoldsOverChains()) {
					serializer.writeHoldsOverChainTag(chain);
				}
			}
		} else if (obj instanceof LinkedObject
				&& tagMapping.equals(OBOConstants.LINK_TAG)) {
			if (obj instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject) obj;
				List<Link> linkList = new LinkedList<Link>();
				linkList.addAll(linkDatabase.getParents(lo));
				Collections.sort(linkList, getLinkComparator(serializer));
				for (Link link : linkList) {
					// CJM: I added this defensive test
					if (!(link instanceof ValueLink))
						writeLink(serializer, link);
				}
				// Iterator it2 = linkList.iterator();
				// while (it2.hasNext()) {
				// Link link = (Link) it2.next();
				// if (link instanceof OBORestriction)
				// writeLink(serializer, link);
				// }
			}
		} else if (obj instanceof ObsoletableObject
				&& tagMapping.equals(OBOConstants.IS_OBSOLETE_TAG)) {
			ObsoletableObject oo = (ObsoletableObject) obj;
			serializer.writeIsObsoleteTag(oo.isObsolete(), oo
					.getObsoleteExtension());
		} else if (obj instanceof ObsoletableObject
				&& tagMapping.equals(OBOConstants.REPLACED_BY_TAG)) {
			ObsoletableObject oo = (ObsoletableObject) obj;

			List scratchList = new LinkedList();
			scratchList.addAll(oo.getReplacedBy());
			Collections.sort(scratchList, getObsoleteComparator(serializer));

			Iterator it2 = scratchList.iterator();
			while (it2.hasNext()) {
				ObsoletableObject replacement = (ObsoletableObject) it2.next();
				serializer.writeReplacedByTag(replacement, oo
						.getReplacedByExtension(replacement));
			}
		} else if (obj instanceof ObsoletableObject
				&& tagMapping.equals(OBOConstants.CONSIDER_TAG)) {
			ObsoletableObject oo = (ObsoletableObject) obj;

			List scratchList = new LinkedList();
			scratchList.addAll(oo.getConsiderReplacements());
			Collections.sort(scratchList, getObsoleteComparator(serializer));

			Iterator it2 = scratchList.iterator();
			while (it2.hasNext()) {
				ObsoletableObject replacement = (ObsoletableObject) it2.next();
				serializer.writeConsiderTag(replacement, oo
						.getConsiderExtension(replacement));
			}
		} else if (tagMapping.equals(OBOConstants.UNRECOGNIZED_TAG)) {
			Iterator it2 = obj.getPropertyValues().iterator();
			while (it2.hasNext()) {
				PropertyValue pv = (PropertyValue) it2.next();
				serializer.writeUnrecognizedTag(pv);
			}
		}
	}

	public void writeLink(OBOSerializer serializer, Link link)
			throws IOException {
		if (allowDangling || !(link.getParent() instanceof DanglingObject)) {
			if (TermUtil.isImplied(link)) {
				if (saveImplied) {
					if (realizeImplied)
						link = realizeLink(link);
					serializer.writeLinkTag(link, link.getNestedValue());
				}
			} else
				serializer.writeLinkTag(link, link.getNestedValue());
		}
	}

	protected OBORestriction realizeLink(Link l) {
		OBORestriction out = new OBORestrictionImpl(l);
		if (l instanceof OBORestriction) {
			OBORestriction link = (OBORestriction) l;
			out.setCardinality(link.getCardinality());
			out.setMaxCardinality(link.getMaxCardinality());
			out.setMinCardinality(link.getMinCardinality());
			out.setCompletes(link.completes());
			out.setInverseCompletes(link.inverseCompletes());
			out.setNecessarilyTrue(link.isNecessarilyTrue());
			out.setInverseNecessarilyTrue(link.isInverseNecessarilyTrue());
		}
		if (l.getNestedValue() != null)
			out.setNestedValue((NestedValue) l.getNestedValue().clone());
		else
			out.setNestedValue(new NestedValueImpl());
		out.getNestedValue().setSuggestedComment(
				"implied link automatically realized");
		return out;
	}

	protected static LinkFilter getPropertyLinkFilter(OBOProperty filterProperty) {
		org.obo.filters.LinkFilter basicLinkFilter = new LinkFilterImpl();
		basicLinkFilter.setAspect(org.obo.filters.LinkFilter.TYPE);

		ObjectFilter idfilter = new ObjectFilterImpl();
		idfilter.setCriterion(new IDSearchCriterion());
		idfilter.setComparison(new EqualsComparison());
		idfilter.setValue(filterProperty.getID());
		basicLinkFilter.setFilter(idfilter);

		return basicLinkFilter;
	}

	public void setReasoner(ReasonedLinkDatabase reasoner) {
		this.reasoner = reasoner;
	}

	public ReasonedLinkDatabase getReasoner() {
		return reasoner;
	}

	public void writeFile(OBOSession session, Filter objectFilter,
			Filter linkFilter, OBOSerializer serializer, PrintStream stream,
			FilteredPath filteredPath) throws IOException,
			CancelledAdapterException {
		String path = filteredPath.getPath();
		allowDangling = filteredPath.getAllowDangling();
		saveImplied = filteredPath.getSaveImplied();
		realizeImplied = filteredPath.getSaveImplied();
		writeModificationData = filteredPath.getWriteModificationData();
		String impliedType = filteredPath.getImpliedType();
		reasonerFactory = filteredPath.getReasonerFactory();

		OBOProperty prefilterProperty = null;
		if (filteredPath.getPrefilterProperty() != null)
			prefilterProperty = (OBOProperty) session.getObject(filteredPath
					.getPrefilterProperty());

		String rootAlgorithm = filteredPath.getRootAlgorithm();

		serializer.setOutputStream(stream);
		serializer.startSerialize();
		for (OBOSerializerExtension extension : extensions) {
			extension.setSerializer(serializer);
			extension.setSerializationEngine(this);
			extension.setSession(session);
			extension.setOutputStream(stream);
			extension.startSerialize();
		}

		boolean saveAll = impliedType.equals(SAVE_ALL);

		LinkDatabase database = new DefaultLinkDatabase(session);
		if (saveImplied || prefilterProperty != null) {
			ReasonedLinkDatabase fullReasoner;
			if (filteredPath.getUseSessionReasoner() && getReasoner() != null) {
				fullReasoner = getReasoner();
			} else {
				fullReasoner = reasonerFactory.createReasoner();
				fullReasoner.setLinkDatabase(new DefaultLinkDatabase(session));
				fullReasoner.recache();
			}

			if (prefilterProperty != null) {
				final FilteredLinkDatabase propertyFiltered = new FilteredLinkDatabase(
						fullReasoner);
				propertyFiltered
						.setFilterMethod(FilteredLinkDatabase.REUSABLE_ITERATOR);
				propertyFiltered
						.setLinkFilter(getPropertyLinkFilter(prefilterProperty));
				if (saveAll) {
					database = propertyFiltered;
				} else {
					final ReasonedLinkDatabase rld = fullReasoner;
					FilteredLinkDatabase trimFiltered = new FilteredLinkDatabase(
							propertyFiltered);
					trimFiltered
							.setFilterMethod(FilteredLinkDatabase.REUSABLE_ITERATOR);
					trimFiltered.setLinkFilter(new Filter() {
						/**
						 * 
						 */
						private static final long serialVersionUID = 1L;

						public boolean satisfies(Object o) {
							if (o instanceof Link) {
								Link link = (Link) o;
								return !ReasonerUtil.shouldBeTrimmedNew(
										propertyFiltered, link);
							} else
								return false;
						}

						public void lock() {
						}

						public void setContext(JexlContext context) {
						}

						public void setReasoner(ReasonedLinkDatabase reasoner) {
						}

						@Override
						public Object clone() {
							return this;
						}
					});
					database = trimFiltered;
				}
			} else {
				if (saveAll)
					database = fullReasoner;
				else {
					database = new TrimmedLinkDatabase(fullReasoner);
				}
			}
			if (database instanceof ReasonedLinkDatabase) {
				((ReasonedLinkDatabase) database).recache(); // why twice?
			}
		}

		if (linkFilter != null || objectFilter != null) {
			database = new FilteredLinkDatabase(database);
			((FilteredLinkDatabase) database).setLinkFilter(linkFilter);
			((FilteredLinkDatabase) database).setTermFilter(objectFilter);
			((FilteredLinkDatabase) database).setAllowDangling(allowDangling);
		}

		if (rootAlgorithm.equals("STRICT")) {
			database = new RootAlgorithmModeratedLinkDatabase(database,
					RootAlgorithm.STRICT);
			((RootAlgorithmModeratedLinkDatabase) database).recache();
		}
		/*
		 * ReasonedLinkDatabase reasonedLinkDatabase = null; if (controller !=
		 * null) { reasonedLinkDatabase = controller.getReasonedLinkDatabase();
		 * useReasoner = controller.getUseReasoner(); } // Start database as a
		 * FilteredReasonedLinkDatabase LinkDatabase database =
		 * reasonedLinkDatabase; if (impliedType.equals("Save all links")) { //
		 * if this is selected make it a PushAndBubbleReasoner database =
		 * reasonedLinkDatabase.getLinkDatabase(); } if (filterPair != null) {
		 * if (!saveImplied || !useReasoner) { database = new
		 * FilteredLinkDatabase(new DefaultLinkDatabase( session));
		 * ((FilteredLinkDatabase) database).setFilterPair(filterPair);
		 * ((FilteredLinkDatabase) database) .setAllowDangling(allowDangling); } }
		 * else { if (!saveImplied || !controller.getUseReasoner()) database =
		 * new DefaultLinkDatabase(session); }
		 */
		setProgressString("Writing header: " + path);
		writeHeader(session, database, serializer, filteredPath);

		setProgressString("Setting up adapter: " + path);

		stanzaOrdering = new LinkedList<StanzaMapping>();
		if (serializer.getStanzaOrdering() == null)
			stanzaOrdering.addAll(OBOConstants.DEFAULT_STANZA_ORDER);
		else
			stanzaOrdering.addAll(serializer.getStanzaOrdering());

		headerTagOrdering = new LinkedList<TagMapping>();
		if (serializer.getHeaderTagOrdering() == null)
			headerTagOrdering.addAll(OBOConstants.DEFAULT_HEADER_TAG_ORDER);
		else
			headerTagOrdering.addAll(serializer.getHeaderTagOrdering());

		tagOrdering = new LinkedList<TagMapping>();
		if (serializer.getTagOrdering() == null)
			tagOrdering.addAll(OBOConstants.DEFAULT_TAG_ORDER);
		else
			tagOrdering.addAll(serializer.getTagOrdering());

		for (OBOSerializerExtension extension : extensions) {
			extension.changeStanzaOrder(stanzaOrdering);
			extension.changeHeaderTagOrder(headerTagOrdering);
			extension.changeTagOrder(tagOrdering);
		}

		Comparator objectComparator = serializer.getObjectComparator();
		if (objectComparator == null)
			objectComparator = OBOConstants.DEFAULT_OBJECT_COMPARATOR;

		Iterator it = stanzaOrdering.iterator();
		while (it.hasNext()) {
			OBOConstants.StanzaMapping stanzaMapping = (OBOConstants.StanzaMapping) it
					.next();
			List objectList = new ArrayList();

			Iterator it2 = database.getObjects().iterator();
			while (it2.hasNext()) {
				IdentifiedObject io = (IdentifiedObject) it2.next();
				if (stanzaMapping.getStanzaClass().isInstance(io)
						&& !io.isBuiltIn()) {
					objectList.add(io);
				}
			}
			setProgressString("Sorting objects: " + path);
			Collections.sort(objectList, objectComparator);

			setProgressString("Writing objects : " + path);
			it2 = objectList.iterator();
			for (int i = 0; it2.hasNext(); i++) {
				if (cancelled)
					doHalt();

				IdentifiedObject io = (IdentifiedObject) it2.next();
				int percent = (100 * i) / objectList.size();
				setProgressValue(percent);
				boolean picked = false;
				for (OBOSerializerExtension extension : extensions) {
					if (extension.writeObject(database, io)) {
						picked = true;
						break;
					}
				}
				if (!picked)
					writeObject(io, database, serializer);
			}
		}
		for (OBOSerializerExtension extension : extensions) {
			extension.endSerialize();
		}
		serializer.endSerialize();
	}

	public String mapID(IdentifiedObject io) {
		return mapID(io, io.getID());
	}

	public String mapID(IdentifiedObject io, String id) {
		String out = id;
		String temp = serializer.mapID(io, id);
		if (temp != null)
			out = temp;
		for (OBOSerializerExtension extension : extensions) {
			temp = extension.mapIDforWrite(io, out);
			if (temp != null)
				out = temp;
		}
		return out;
	}

	public void doHalt() throws CancelledAdapterException {
		throw new CancelledAdapterException();
	}

	public void addSerializerExtension(OBOSerializerExtension extension)
			throws IOException {
		extensions.add(extension);
	}

	public void removeSerializerExtension(OBOSerializerExtension extension)
			throws IOException {
		extensions.remove(extension);
	}

	public IDProfile getCurrentProfile() {
		return currentProfile;
	}

	public void setCurrentProfile(IDProfile currentProfile) {
		this.currentProfile = currentProfile;
	}

	public String getAutogenString() {
		return autogenString;
	}

	public void setAutogenString(String autogenString) {
		this.autogenString = autogenString;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}
}
