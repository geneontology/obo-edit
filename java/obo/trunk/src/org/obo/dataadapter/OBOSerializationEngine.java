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
import org.obo.identifier.IDProfile;
import org.obo.identifier.IDRule;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerFactory;
import org.obo.reasoner.impl.TrimmedLinkDatabase;
import org.obo.reasoner.rbr.RuleBasedReasonerFactory;
import org.obo.util.ReasonerUtil;
import org.obo.util.TermUtil;

import java.io.*;
import java.util.*;

import org.apache.log4j.*;

/**
 * Engine to write files in OBO format. Provides writeList to OBOSerializer to write file.
 *
 * ** DO NOT EDIT without prior notification to aabdulla@berkeleybop.org or cjm@berkeleybop.org **
 *
 */
public class OBOSerializationEngine extends AbstractProgressValued {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OBOSerializationEngine.class);

	protected static int DONT_WRITE_ID_RULES = 0;

	protected static int WRITE_STORED_ID_RULES = 1;

	protected static int WRITE_CURRENT_ID_RULES = 2;

	public static final String SAVE_TRIMMED_LINKS = "Save trimmed links";

	public static final String SAVE_ALL = "Save all links";

	public static class FilteredPath {

		protected Filter linkFilter;

		protected Filter objectFilter;

		protected TagFilter tagFilter;

		protected String prefilterProperty;

		protected boolean doFilter = false;

		protected boolean doLinkFilter = false;

		// set Tag filtering TRUE by default
		protected boolean doTagFilter = false;

		protected boolean allowDangling = false;

		protected boolean followIsaClosure = false;

		protected boolean saveImplied = false;

		// FCR does not appear to work with regulation examples...
		protected ReasonerFactory reasonerFactory = new 	RuleBasedReasonerFactory();

		protected boolean discardUnusedCategories = false;

		protected boolean useSessionReasoner = true;

		protected int idRuleMode = DONT_WRITE_ID_RULES;

		protected String impliedType = SAVE_TRIMMED_LINKS;

		protected String path;

		protected String remark;

		protected String rootAlgorithm = "GREEDY";

		protected boolean assertImpliedLinks = false;

		protected boolean saveTypes;

		protected boolean writeModificationData = false;

		protected HashSet<OBOConstants.TagMapping> tagsToWrite;
		
		protected boolean includeXrefDesc = false;

		public FilteredPath() {
		}

		public FilteredPath(Filter linkFilter, Filter objectFilter, String path) {
			setLinkFilter(linkFilter);
			setObjectFilter(objectFilter);
			setPath(path);
		}

		public FilteredPath(Filter linkFilter, Filter objectFilter, TagFilter tagFilter, String path) {
			setLinkFilter(linkFilter);
			setObjectFilter(objectFilter);			
			setTagFilter(tagFilter);
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

		public void setIsaClosure(boolean isaClosure) {
			this.followIsaClosure = isaClosure;
		}

		public boolean getIsaClosure() {
			return followIsaClosure;
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

		public boolean getDoTagFilter() {
			return doTagFilter;
		}

		public void setDoLinkFilter(boolean doLinkFilter) {
			this.doLinkFilter = doLinkFilter;
		}

		public void setDoTagFilter(boolean doTagFilter) {
			this.doTagFilter = doTagFilter;
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

		public boolean getAssertImpliedLinks() {
			return assertImpliedLinks;
		}

		public void setAssertImpliedLinks(boolean assertImpliedLinks) {
			this.assertImpliedLinks = assertImpliedLinks;
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

		public TagFilter getTagFilter() {
			return tagFilter;
		}

		public void setTagFilter(TagFilter tagFilter) {
			this.tagFilter = tagFilter;
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

		public void setTagsToWrite(HashSet<OBOConstants.TagMapping> tagsToWrite) {
			this.tagsToWrite = tagsToWrite;
		}

		public HashSet<OBOConstants.TagMapping> getTagsToWrite(){
			return tagsToWrite;
		}

		public boolean getIncludeXrefDesc() {
			return includeXrefDesc;
		}

		public void setIncludeXrefDesc(boolean includeXrefDesc) {
			this.includeXrefDesc = includeXrefDesc;
		}

	}

	protected boolean cancelled = false;

	protected List streams = new LinkedList();

	protected List closingStreams = new LinkedList();

	protected List progressListeners = new LinkedList();

	protected List<OBOSerializerExtension> extensions = new LinkedList<OBOSerializerExtension>();

	protected OBOSerializer serializer;

	protected boolean allowDangling;

	protected boolean followIsaClosure;

	protected boolean saveImplied;

	protected ReasonerFactory reasonerFactory;

	protected boolean assertImplied;

	protected List<TagMapping> tagOrdering;

	protected List<TagMapping> engineTagOrderingList;

	protected List<StanzaMapping> stanzaOrdering;

	protected List<TagMapping> headerTagOrdering;

	protected ReasonedLinkDatabase reasoner;

	protected IDProfile currentProfile;
	protected String autogenString;
	protected String username;

	protected boolean writeModificationData;
	
	protected boolean includeXrefDesc;

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
		for(Object o : streams){
			SafeFileOutputStream sfos = (SafeFileOutputStream) o;
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
				null, null, null, path)));
	}

	public void serialize(OBOSession session, OBOSerializer serializer,
			Collection<FilteredPath> filteredPaths) throws DataAdapterException {
		setSerializer(serializer);
		streams.clear();
		cancelled = false;
		for(FilteredPath filteredPath : filteredPaths){
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
				SafeFileOutputStream sfos = new SafeFileOutputStream(filteredPath.getPath());
				streams.add(sfos);
				PrintStream stream = new PrintStream(new BufferedOutputStream(sfos), false, OBOConstants.DEFAULT_CHARACTER_ENCODING);
				closingStreams.add(stream);

				Filter linkFilter = filteredPath.getLinkFilter();
				Filter objectFilter = filteredPath.getObjectFilter();
				HashSet<OBOConstants.TagMapping> tagFilter = null;
				if(filteredPath.getTagFilter() != null)
					tagFilter = filteredPath.getTagFilter().getTagsToWrite();

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
				if (!filteredPath.getDoTagFilter())
					tagFilter = null;

                                //				logger.debug("OBOSerializationEngine.serialize initiating writeFile.. checking tagFilter: " + tagFilter);
				writeFile(session, objectFilter, linkFilter, tagFilter, serializer, stream, filteredPath);
			} catch (IOException ex) {
				throw new DataAdapterException("Write error", ex);
			}
		}
		for(Object o : closingStreams){
			OutputStream os = (OutputStream) o;
			try {
				os.close();
			} catch (IOException ex) {
				throw new DataAdapterException("Could not commit changes to " + os);
			}
			catch (Exception e) {
				e.printStackTrace();
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

		for(Object tagSpec : headerTagOrdering){
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
					for(IdentifiedObject io : linkDatabase.getObjects()){
						if (io instanceof SubsetObject) {
							SubsetObject co = (SubsetObject) io;
							usedCategories.addAll(co.getSubsets());
						}
					}
					scratchList.addAll(usedCategories);
				} else
					scratchList.addAll(session.getSubsets());
				Collections.sort(scratchList, comparator);
				for(Object o: scratchList){
					TermSubset cat = (TermSubset) o;
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
					for(IdentifiedObject io : linkDatabase.getObjects()){
						if (io instanceof SynonymedObject) {
							SynonymedObject so = (SynonymedObject) io;
							for(Synonym s : so.getSynonyms()){
								if (s.getSynonymType() != null)
									usedCategories.add(s.getSynonymType());
							}
						}
					}
					scratchList.addAll(usedCategories);
				} else
					scratchList.addAll(session.getSynonymTypes());
				Collections.sort(scratchList, comparator);
				for(Object o : scratchList){
					SynonymType cat = (SynonymType) o;
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
						for(IDRule rule : profile.getRules()){
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
						for(Object o : nsList){
							String ns = (String) o;
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
		//		logger.debug("\n writing object: " + obj);
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
				//logger.info("OBOSerializationEngine.writeObject: not writing bogus object " + obj); // DEL
				return;
			}
		}


		//ordering collection to figure out which tags will be written in the file. 
		for(Object o: engineTagOrderingList){
			OBOConstants.TagMapping tagMapping = (OBOConstants.TagMapping) o;
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
	} //writeObject

	/** Don't write out bogus objects (which would come out as "[null]" stanzas).
	(Is this the right test?) */
	protected boolean isRealObject(IdentifiedObject obj) {
		if ((   TermUtil.isClass(obj) ||
				TermUtil.isProperty(obj) ||
				TermUtil.isInstance(obj) 
		) &&
		!TermUtil.isDangling(obj)) 
			return true;
		else
			return false;
	}

	protected void writeTag(TagMapping tagMapping, IdentifiedObject obj,
			LinkDatabase linkDatabase, OBOSerializer serializer)
	throws IOException, CancelledAdapterException {
		writeTag(tagMapping, obj, linkDatabase, serializer, allowDangling, followIsaClosure,
				assertImplied, writeModificationData, includeXrefDesc);
	}

	protected void writeTag(TagMapping tagMapping, IdentifiedObject obj,
			LinkDatabase linkDatabase, OBOSerializer serializer,
			boolean allowDangling, boolean doIsaClosure, 
			boolean realizeImpliedLinks, boolean writeModificationData,
			boolean includeXrefDesc) throws IOException, CancelledAdapterException {
		//		logger.debug("OBOSerializationEngine.writeTag -- obj:  " + obj + "  tag: " + tagMapping);
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
			for(Object o : scratchList){
				String id = (String) o;
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
					scratchList, includeXrefDesc, ((DefinedObject) obj)
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
		} else if (obj instanceof SubsetObject
				&& tagMapping.equals(OBOConstants.SUBSET_TAG)) {

			List scratchList = new LinkedList();
			scratchList.addAll(((SubsetObject) obj).getSubsets());
			Collections.sort(scratchList, getCategoryComparator(serializer));
			for(Object o : scratchList){
				TermSubset category = (TermSubset) o;
				NestedValue nv = ((SubsetObject) obj)
				.getCategoryExtension(category);
				serializer.writeSubsetTag(category, nv);
			}
		} else if (obj instanceof SynonymedObject
				&& tagMapping.equals(OBOConstants.RELATED_SYNONYM_TAG)) {

			List scratchList = new LinkedList();
			scratchList.addAll(((SynonymedObject) obj).getSynonyms());
			Collections.sort(scratchList, getSynonymComparator(serializer));
			for(Object o : scratchList){
				Synonym synonym = (Synonym) o;
				serializer.writeSynonymTag(synonym, includeXrefDesc, synonym.getNestedValue());
			}
		} else if (obj instanceof DbxrefedObject
				&& tagMapping.equals(OBOConstants.XREF_TAG)) {
			List scratchList = new LinkedList();
			scratchList.addAll(((DbxrefedObject) obj).getDbxrefs());
			Collections.sort(scratchList, getDbxrefComparator(serializer));
			for(Object o : scratchList){
				Dbxref dbxref = (Dbxref) o;
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
				for(Object o : linkList){
					Link link = (Link) o;
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
			// this is handled by generic link writing
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
		} 

		else if (obj instanceof LinkedObject && tagMapping.equals(OBOConstants.LINK_TAG)) {
			if (obj instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject) obj;
				List<Link> linkList = new LinkedList<Link>();

				Collection<Link> parents;
				// followIsaClosure will typically be combined with filtering objects over some condition - The link database here is thus a FilteredLinkDatabase
				// To obtain all the links (including the ones that do not comply with the filter conditions) for the closure, 
				// get all parents from the linked object instead of accessing them through the linkDatabase.
				if(followIsaClosure)
					parents = lo.getParents();
				else 
					parents = linkDatabase.getParents(lo);


				for (Link p : parents) {
					if (p.getParent() == null) {
						logger.error("invalid link: "+p+" Child: "+p.getChild().getClass());
					}
					if (p.getType() == null) {
						logger.error("invalid link: "+p+" Child: "+p.getChild().getClass());
					}
				}
				linkList.addAll(parents);
				Collections.sort(linkList, getLinkComparator(serializer));
				for (Link link : linkList) {
					// CJM: I added this defensive test
					if (!(link instanceof ValueLink))
						writeLink(serializer, link, linkDatabase);
				}
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
			for(Object o : scratchList){
				ObsoletableObject replacement = (ObsoletableObject) o;
				serializer.writeReplacedByTag(replacement, oo
						.getReplacedByExtension(replacement));
			}
		} else if (obj instanceof ObsoletableObject
				&& tagMapping.equals(OBOConstants.CONSIDER_TAG)) {
			ObsoletableObject oo = (ObsoletableObject) obj;

			List scratchList = new LinkedList();
			scratchList.addAll(oo.getConsiderReplacements());
			Collections.sort(scratchList, getObsoleteComparator(serializer));
			for(Object o : scratchList){
				ObsoletableObject replacement = (ObsoletableObject) o;
				serializer.writeConsiderTag(replacement, oo
						.getConsiderExtension(replacement));
			}
		} 
		else if (tagMapping.equals(OBOConstants.IS_METADATA_TAG) && obj instanceof OBOProperty) {
			serializer.writeIsMetadataTag(((OBOProperty)obj).isMetadataTag(), null);
		}
		else if (tagMapping.equals(OBOConstants.UNRECOGNIZED_TAG)) {
			for(PropertyValue pv : obj.getPropertyValues()){
				serializer.writeUnrecognizedTag(pv);
			}
		}
	}

	public void writeLink(OBOSerializer serializer, Link link, LinkDatabase database)
	throws IOException, CancelledAdapterException {
		if (allowDangling || !(link.getParent() instanceof DanglingObject)) {
			if (TermUtil.isImplied(link)) {
				if (saveImplied) {
					if (assertImplied)
						link = assertLink(link);
					serializer.writeLinkTag(link, link.getNestedValue());
				}
			} else
				serializer.writeLinkTag(link, link.getNestedValue());
		}
	}

	protected OBORestriction assertLink(Link l) {
		OBORestriction out = new OBORestrictionImpl(l);
		if (l instanceof OBORestriction) {
			OBORestriction link = (OBORestriction) l;
			out.setCardinality(link.getCardinality());
			out.setMaxCardinality(link.getMaxCardinality());
			out.setMinCardinality(link.getMinCardinality());
			out.setCompletes(link.getCompletes());
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
			Filter linkFilter, HashSet<TagMapping> tagFilter, OBOSerializer serializer, PrintStream stream,
			FilteredPath filteredPath) throws IOException,
			CancelledAdapterException {
		String path = filteredPath.getPath();
		allowDangling = filteredPath.getAllowDangling();
		followIsaClosure = filteredPath.getIsaClosure();
		saveImplied = filteredPath.getSaveImplied();
		assertImplied = filteredPath.getSaveImplied();
		writeModificationData = filteredPath.getWriteModificationData();
		includeXrefDesc = filteredPath.getIncludeXrefDesc();
		String impliedType = filteredPath.getImpliedType();
		reasonerFactory = filteredPath.getReasonerFactory();

		OBOProperty prefilterProperty = null;
		if (filteredPath.getPrefilterProperty() != null)
			prefilterProperty = (OBOProperty) session.getObject(filteredPath.getPrefilterProperty());

                String comment = session.getCurrentHistory().getComment();
                if (objectFilter != null && (comment == null || comment.equals(""))) {
		    logger.debug("writeFile: objectFilter = " + objectFilter);
                    comment = "Filtered by " + objectFilter.toString() + (followIsaClosure ? " (follow is_a closure)" : "");
                    session.getCurrentHistory().setComment(comment);
                }

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

		LinkDatabase database = session.getLinkDatabase();

		if (saveImplied || prefilterProperty != null || requiresReasoner(objectFilter)) {
			ReasonedLinkDatabase fullReasoner;
			if (filteredPath.getUseSessionReasoner() && getReasoner() != null) {
				fullReasoner = getReasoner();
			} else {
                            // If filtered save requires reasoner and reasoner isn't currently on,
                            // start a new one.
                            logger.info("User requested a filtered save that requires the reasoner (" +
                                        objectFilter + ") but reasoner isn't on.  Starting reasoner--this may take a while.");
                            setProgressString("This filtered save requires the reasoner--please be patient.");
				fullReasoner = reasonerFactory.createReasoner();
				fullReasoner.setLinkDatabase(new DefaultLinkDatabase(session));
				fullReasoner.recache();
                                setProgressString("Filtering objects...");
			}

			if (prefilterProperty != null) {
                            setProgressString("Prefiltering...");
				final FilteredLinkDatabase propertyFiltered = new FilteredLinkDatabase(
						fullReasoner);
				propertyFiltered
				.setFilterMethod(FilteredLinkDatabase.REUSABLE_ITERATOR);
				propertyFiltered
				.setLinkFilter(getPropertyLinkFilter(prefilterProperty));
				if (saveAll) {
					database = propertyFiltered;
				} else {
                                    setProgressString("Filtering links...");
					final ReasonedLinkDatabase rld = fullReasoner;
					FilteredLinkDatabase trimFiltered = new FilteredLinkDatabase(
							propertyFiltered);
					trimFiltered
					.setFilterMethod(FilteredLinkDatabase.REUSABLE_ITERATOR);
					trimFiltered.setLinkFilter(new Filter() {

						private static final long serialVersionUID = 1L;

						public boolean satisfies(Object o) {
							if (o instanceof Link) {
								Link link = (Link) o;
								return !ReasonerUtil.shouldBeTrimmedNew(propertyFiltered, link);
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
				((ReasonedLinkDatabase) database).recache(); 
			}
		}

		if (linkFilter != null || objectFilter != null) {
			database = new FilteredLinkDatabase(database);
			((FilteredLinkDatabase) database).setLinkFilter(linkFilter);
			((FilteredLinkDatabase) database).setTermFilter(objectFilter);
			((FilteredLinkDatabase) database).setAllowDangling(allowDangling);
			((FilteredLinkDatabase) database).setFollowIsaClosure(followIsaClosure);
		}

		setProgressString("Collecting objects to write to file: " + path);
		Collection writeList = new ArrayList();
		Collection filteredObjects = database.getObjects();
		logger.debug("filteredObjects.size(): " + filteredObjects.size());

		for(Object o : filteredObjects){
			writeList.add(o);
		}

		// If requested, include is_a closure for cross referenced terms
		//List of all referenced objects that are not present in the original writeList (will contain duplicate entries -- adding to a HashSet later to remove redundant objects)
		Collection allRefsList = new ArrayList();
		if( database.getClass().getSimpleName().equals("FilteredLinkDatabase") && (((FilteredLinkDatabase) database).getFollowIsaClosure()) ){
                    for(Object obj : writeList){
                        if (cancelled)
                            doHalt();
                        LinkedObject lo = (LinkedObject) obj;
                        //if term has intersection links...
                        if(TermUtil.isIntersection(lo)){
                            for(Link link : lo.getParents()){
                                //check if this is an  intersection link
                                // (Include all intersections, not just is_a intersections)
                                if(TermUtil.isIntersection(link)) { // && link.getType().equals(OBOProperty.IS_A)) {
                                    // logger.debug("is_intersection: lo = " + lo + ", link = " + link + ", type = " + link.getType()); // DEL
                                    //get ancestors of cross referenced parent term
                                    // for(Object o : TermUtil.getAncestors(link.getParent(), null)){
                                    for(Object o :
                                            TermUtil.getisaAncestors(link.getParent(),
                                                                     true)) {
                                        // logger.debug("For link " + link + ", IsAancestors = " + TermUtil.getisaAncestors(link.getParent())); // DEL
                                        IdentifiedObject refParent = (IdentifiedObject) o;
                                        //get ancestors of cross referenced child term
                                        // for(Object co : TermUtil.getAncestors(link.getChild(), null)){
                                        for(Object co : TermUtil.getisaAncestors(link.getChild(), true)){
                                            IdentifiedObject refChild = (IdentifiedObject) co;
                                            // logger.debug("For link " + link + ", refParent = " + refParent + ", refChild = " + refChild); // DEL
									
                                            //check if terms exists in filteredObjects. filo: filtered object, filio: filtered identified object
                                            boolean pexists = false;
                                            boolean cexists = false;
                                            for(Object filo : writeList){
                                                IdentifiedObject filio = (IdentifiedObject) filo;
                                                if(filio.getName().equals(refParent.getName()))
                                                    pexists = true;		
                                                if(filio.getName().equals(refChild.getName()))
                                                    cexists = true;
                                            }								
                                            if(!pexists) {
                                                allRefsList.add(refParent);
                                            }
                                            if(!cexists) {
                                                allRefsList.add(refChild);
                                            }
                                            // Broken version from r3092
                                            // for(Object co : TermUtil.getisaAncestors(link.getChild())){
                                            // 	IdentifiedObject refChild = (IdentifiedObject) co;					
                                            // 	allRefsSet.add(refChild);
                                            // 	allRefsSet.add(refParent);
                                        }
                                    }
                                }// isIntersection(link)
                            }
                        } 
                    }

                    //remove duplicate objects from allRefsList
                    HashSet refshs = new HashSet(allRefsList);
                    for(Object o : refshs){
                        writeList.add(o);
                    }
		}

		if (rootAlgorithm.equals("STRICT")) {
			database = new RootAlgorithmModeratedLinkDatabase(database,
					RootAlgorithm.STRICT);
			((RootAlgorithmModeratedLinkDatabase) database).recache();
		}

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

		//populating tags
		engineTagOrderingList = new LinkedList<TagMapping>();
		if (serializer.getTagOrdering() == null  ){
			engineTagOrderingList.addAll(OBOConstants.DEFAULT_TAG_ORDER);
		} else {
			if(filteredPath.getTagFilter() != null){
				serializer.setTagOrdering(filteredPath.getTagFilter().getTagsToWrite());
				engineTagOrderingList.addAll(serializer.getTagOrdering());
			}
			if(filteredPath.getTagFilter() == null)
				engineTagOrderingList.addAll(OBOConstants.DEFAULT_TAG_ORDER);
		}

		for (OBOSerializerExtension extension : extensions) {
			extension.changeStanzaOrder(stanzaOrdering);
			extension.changeHeaderTagOrder(headerTagOrdering);
			extension.changeTagOrder(engineTagOrderingList);
		}

		Comparator objectComparator = serializer.getObjectComparator();
		if (objectComparator == null)
			objectComparator = OBOConstants.DEFAULT_OBJECT_COMPARATOR;
		for(Object o : stanzaOrdering){
			OBOConstants.StanzaMapping stanzaMapping = (OBOConstants.StanzaMapping) o;

			//compiling writeObjects from writeList
			List writeObjects = new ArrayList();
			for(Object obj : writeList){
				IdentifiedObject io = (IdentifiedObject) obj;
				if(stanzaMapping.stanzaName.equalsIgnoreCase("Typdef")){
					if (stanzaMapping.getStanzaClass().isInstance(io) && !io.isBuiltIn())
						writeObjects.add(io);
				}
				if(!stanzaMapping.stanzaName.equalsIgnoreCase("Typdef")){
					if (stanzaMapping.getStanzaClass().isInstance(io) && !io.isBuiltIn())
						writeObjects.add(io);
				}

			}
			setProgressString("Sorting objects");
			Collections.sort(writeObjects, objectComparator);

			setProgressString("Writing objects to file: " + path);

			for(Object writeo : writeObjects){
				IdentifiedObject io = (IdentifiedObject) writeo;
				//logger.debug("writing object " + io);
				writeObject(io,database,serializer);
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

    /** I'd rather not rely on the filter's string, but there's no
        Filter.getAspect method (there's one in.ObjectFilter, but we just have a Filter). */
    private boolean requiresReasoner(Filter filter) {
        if (filter == null)
            return false;

        if ((filter.toString().indexOf("Ancestor") >= 0) ||
            (filter.toString().indexOf("Descend") >= 0))
            return true;
        else
            return false;
    }

}
