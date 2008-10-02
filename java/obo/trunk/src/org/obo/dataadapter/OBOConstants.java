package org.obo.dataadapter;

import org.obo.datamodel.*;
import org.obo.util.TermUtil;

import java.util.*;
import java.util.regex.Pattern;

import org.apache.log4j.*;

public class OBOConstants {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OBOConstants.class);

	private OBOConstants() {
	}

	public static class TagMapping {
		protected String tagName;

		protected Class allowsClass;

		protected OBOSerializerExtension provider;

		public TagMapping(String tagName, Class allowsClass) {
			this(tagName, allowsClass, null);
		}

		public TagMapping(String tagName, Class allowsClass,
				OBOSerializerExtension provider) {
			this.tagName = tagName;
			this.allowsClass = allowsClass;
			this.provider = provider;
		}
		
		public String getName() {
			return tagName;
		}

		@Override
		public String toString() {
			return tagName;
		}
	}

	public static class StanzaMapping {
		protected String stanzaName;

		protected Class forClass;

		public StanzaMapping(String stanzaName, Class forClass) {
			this.stanzaName = stanzaName;
			this.forClass = forClass;
		}

		public Class getStanzaClass() {
			return forClass;
		}

		@Override
		public String toString() {
			return stanzaName;
		}
	}

	private static final List _defaultStanzaOrder = new LinkedList();

	public static final List DEFAULT_STANZA_ORDER = Collections
			.unmodifiableList(_defaultStanzaOrder);

	private static final List _defaultHeaderTagOrder = new LinkedList();

	public static final List DEFAULT_HEADER_TAG_ORDER = Collections
			.unmodifiableList(_defaultHeaderTagOrder);

	private static final List _defaultTagOrder = new LinkedList();

	public static final List DEFAULT_TAG_ORDER = Collections
			.unmodifiableList(_defaultTagOrder);

	public static final Comparator DEFAULT_OBJECT_COMPARATOR = new Comparator() {
		public int compare(Object a, Object b) {
			return DEFAULT_ID_COMPARATOR.compare(
					((IdentifiedObject) a).getID(), ((IdentifiedObject) b)
							.getID());
		}
	};

	public static final Comparator DEFAULT_ID_COMPARATOR = new Comparator() {
		public int compare(Object a, Object b) {
			return ((String) a).compareToIgnoreCase((String) b);
		}
	};

	public static final Comparator DEFAULT_OBSOLETE_COMPARATOR = new Comparator() {
		public int compare(Object a, Object b) {
			return DEFAULT_ID_COMPARATOR.compare(
					((IdentifiedObject) a).getID(), ((IdentifiedObject) b)
							.getID());
		}
	};

	public static final Comparator DEFAULT_DBXREF_COMPARATOR = Dbxref.COMPARATOR;

	/*
	 * new Comparator() { public int compare(Object a, Object b) { Dbxref da =
	 * (Dbxref) a; Dbxref db = (Dbxref) b; return a.toString().
	 * compareToIgnoreCase(b.toString()); } };
	 */

	public static final Comparator DEFAULT_RELATED_SYNONYM_COMPARATOR = Synonym.COMPARATOR;

	public static final Comparator DEFAULT_LINK_COMPARATOR = new Comparator() {
		protected int getTypeRanking(OBOProperty property, boolean completes) {
			if (property.equals(OBOProperty.IS_A)) {
				if (completes)
					return 1;
				else
					return 0;
			} else if (completes) {
				return 2;
			} else if (property.equals(OBOProperty.UNION_OF))
				return 3;
			else if (property.equals(OBOProperty.DISJOINT_FROM))
				return 4;
			else if (property.equals(OBOProperty.INVERSE_OF))
				return 5;
			else
				return 6;
		}

		public int compare(Object a, Object b) {
			Link la = (Link) a;
			Link lb = (Link) b;

			int compVal = getTypeRanking(la.getType(), TermUtil
					.isIntersection(la))
					- getTypeRanking(lb.getType(), TermUtil.isIntersection(lb));
			if (compVal == 0) {
				compVal = DEFAULT_ID_COMPARATOR.compare(la.getType().getID(),
						lb.getType().getID());
				if (compVal == 0) {
					return DEFAULT_ID_COMPARATOR.compare(
							la.getParent().getID(), lb.getParent().getID());
				} else
					return compVal;
			} else
				return compVal;
		}
	};

	public static final Comparator DEFAULT_RELATED_SYNONYM_CATEGORY_COMPARATOR = new Comparator() {
		public int compare(Object a, Object b) {
			return DEFAULT_ID_COMPARATOR.compare(((SynonymCategory) a).getID(),
					((SynonymCategory) b).getID());
		}
	};

	public static final Comparator DEFAULT_CATEGORY_COMPARATOR = new Comparator() {
		public int compare(Object a, Object b) {
			return DEFAULT_ID_COMPARATOR.compare(((TermCategory) a).getName(),
					((TermCategory) b).getName());
		}
	};

	protected static Pattern identifierPattern = Pattern
			.compile("[A-Za-z0-9_\\-]*");

	/**
	 * Returns true if the input string is a valid OBO identifier. OBO
	 * identifiers can only contain letters, numbers, underscores and dashes.
	 * 
	 * @param string
	 * @return
	 */
	public static boolean isOBOIdentifierToken(String string) {
		return identifierPattern.matcher(string).matches();
	}

	public static final StanzaMapping TERM_STANZA = new StanzaMapping("Term",
			OBOClass.class);

	public static final StanzaMapping TYPEDEF_STANZA = new StanzaMapping(
			"Typdef", OBOProperty.class);

	public static final StanzaMapping INSTANCE_STANZA = new StanzaMapping(
			"Instance", Instance.class);

	public static final String FORMAT_VERSION_HEADER_TAG = "format-version";

	public static final String DATA_VERSION_HEADER_TAG = "data-version";

	public static final String DATE_HEADER_TAG = "date";

	public static final String SAVED_BY_HEADER_TAG = "saved-by";

	public static final String AUTO_GENERATED_BY_HEADER_TAG = "autogenerated-by";

	public static final String SUBSETDEF_HEADER_TAG = "subsetdef";

	public static final String IDSPACE_HEADER_TAG = "idspace";

	public static final String RELATED_SYNONYMTYPEDEF_HEADER_TAG = "synonymtypedef";

	public static final String DEFAULT_NAMESPACE_HEADER_TAG = "default-namespace";

	public static final String NAMESPACE_ID_RULE_HEADER_TAG = "namespace-id-rule";

	public static final String REMARK_HEADER_TAG = "remark";
		
	public static final String IMPORT_HEADER_TAG = "import";

	public static final TagMapping ID_TAG = new TagMapping("id",
			IdentifiedObject.class);

	public static final TagMapping IS_ANONYMOUS_TAG = new TagMapping(
			"is_anonymous", IdentifiedObject.class);

	public static final TagMapping NAME_TAG = new TagMapping("name",
			IdentifiedObject.class);

	public static final TagMapping NAMESPACE_TAG = new TagMapping("namespace",
			IdentifiedObject.class);

	public static final TagMapping ALT_ID_TAG = new TagMapping("alt_id",
			MultiIDObject.class);

	public static final TagMapping DEF_TAG = new TagMapping("def",
			DefinedObject.class);

	public static final TagMapping COMMENT_TAG = new TagMapping("comment",
			CommentedObject.class);

	public static final TagMapping SUBSET_TAG = new TagMapping("subset",
			CategorizedObject.class);

	public static final TagMapping RELATED_SYNONYM_TAG = new TagMapping(
			"synonym", SynonymedObject.class);

	public static final TagMapping XREF_TAG = new TagMapping("xref",
			DbxrefedObject.class);

	public static final TagMapping INSTANCE_OF_TAG = new TagMapping(
			"instance_of", Instance.class);

	public static final TagMapping PROPERTY_VALUE_TAG = new TagMapping(
			"property_value", Instance.class);

	public static final TagMapping DOMAIN_TAG = new TagMapping("domain",
			OBOProperty.class);

	public static final TagMapping RANGE_TAG = new TagMapping("range",
			OBOProperty.class);

	public static final TagMapping IS_CYCLIC_TAG = new TagMapping("is_cyclic",
			OBOProperty.class);

	public static final TagMapping IS_REFLEXIVE_TAG = new TagMapping(
			"is_reflexive", OBOProperty.class);

	public static final TagMapping ALWAYS_IMPLIES_INVERSE_TAG = new TagMapping(
			"always_implies_inverse", OBOProperty.class);

	public static final TagMapping IS_SYMMETRIC_TAG = new TagMapping(
			"is_symmetric", OBOProperty.class);	
	public static final TagMapping TRANSITIVE_OVER_TAG = new TagMapping(
			"transitive_over", OBOProperty.class);
	public static final TagMapping HOLDS_OVER_CHAIN_TAG = new TagMapping(
			"holds_over_chain", OBOProperty.class);

	public static final TagMapping IS_TRANSITIVE_TAG = new TagMapping(
			"is_transitive", OBOProperty.class);

	public static final TagMapping LINK_TAG = new TagMapping("link",
			LinkedObject.class);

	public static final TagMapping IS_OBSOLETE_TAG = new TagMapping(
			"is_obsolete", ObsoletableObject.class);

	public static final TagMapping REPLACED_BY_TAG = new TagMapping(
			"replaced_by", ObsoletableObject.class);

	public static final TagMapping CONSIDER_TAG = new TagMapping("consider",
			ObsoletableObject.class);

	public static final TagMapping UNRECOGNIZED_TAG = new TagMapping("*",
			IdentifiedObject.class);

	public static final TagMapping VALUE_LINK_TAG = new TagMapping(
			"property_value", Instance.class);

	public static final TagMapping CREATED_BY_TAG = new TagMapping(
			"created_by", ModificationMetadataObject.class);
	
	public static final TagMapping CREATION_DATE_TAG = new TagMapping(
			"creation_date", ModificationMetadataObject.class);
	
	public static final TagMapping MODIFIED_BY_TAG = new TagMapping(
			"modified_by", ModificationMetadataObject.class);
	
	public static final TagMapping MODIFICATION_DATE_TAG = new TagMapping(
			"modification_date", ModificationMetadataObject.class);

	static {
		_defaultHeaderTagOrder.add(FORMAT_VERSION_HEADER_TAG);
		_defaultHeaderTagOrder.add(DATA_VERSION_HEADER_TAG);
		_defaultHeaderTagOrder.add(DATE_HEADER_TAG);
		_defaultHeaderTagOrder.add(SAVED_BY_HEADER_TAG);
		_defaultHeaderTagOrder.add(AUTO_GENERATED_BY_HEADER_TAG);
		_defaultHeaderTagOrder.add(SUBSETDEF_HEADER_TAG);
		_defaultHeaderTagOrder.add(RELATED_SYNONYMTYPEDEF_HEADER_TAG);
		_defaultHeaderTagOrder.add(DEFAULT_NAMESPACE_HEADER_TAG);
		_defaultHeaderTagOrder.add(NAMESPACE_ID_RULE_HEADER_TAG);
		_defaultHeaderTagOrder.add(IDSPACE_HEADER_TAG);
		_defaultHeaderTagOrder.add(IMPORT_HEADER_TAG);

		_defaultHeaderTagOrder.add(REMARK_HEADER_TAG);

		_defaultStanzaOrder.add(TERM_STANZA);
		_defaultStanzaOrder.add(TYPEDEF_STANZA);
		_defaultStanzaOrder.add(INSTANCE_STANZA);

		_defaultTagOrder.add(ID_TAG);
		_defaultTagOrder.add(IS_ANONYMOUS_TAG);
		_defaultTagOrder.add(NAME_TAG);
		_defaultTagOrder.add(NAMESPACE_TAG);
		_defaultTagOrder.add(ALT_ID_TAG);
		_defaultTagOrder.add(DEF_TAG);
		_defaultTagOrder.add(COMMENT_TAG);
		_defaultTagOrder.add(SUBSET_TAG);
		_defaultTagOrder.add(RELATED_SYNONYM_TAG);
		_defaultTagOrder.add(XREF_TAG);
		_defaultTagOrder.add(INSTANCE_OF_TAG);
		_defaultTagOrder.add(PROPERTY_VALUE_TAG);
		_defaultTagOrder.add(DOMAIN_TAG);
		_defaultTagOrder.add(RANGE_TAG);
		_defaultTagOrder.add(ALWAYS_IMPLIES_INVERSE_TAG);
		_defaultTagOrder.add(IS_CYCLIC_TAG);
		_defaultTagOrder.add(IS_REFLEXIVE_TAG);
		_defaultTagOrder.add(IS_SYMMETRIC_TAG);
		_defaultTagOrder.add(IS_TRANSITIVE_TAG);
		_defaultTagOrder.add(TRANSITIVE_OVER_TAG);
		_defaultTagOrder.add(HOLDS_OVER_CHAIN_TAG);
		_defaultTagOrder.add(LINK_TAG);
		_defaultTagOrder.add(VALUE_LINK_TAG);
		_defaultTagOrder.add(IS_OBSOLETE_TAG);
		_defaultTagOrder.add(REPLACED_BY_TAG);
		_defaultTagOrder.add(CONSIDER_TAG);
		_defaultTagOrder.add(UNRECOGNIZED_TAG);
		_defaultTagOrder.add(CREATED_BY_TAG);
		_defaultTagOrder.add(CREATION_DATE_TAG);
		_defaultTagOrder.add(MODIFIED_BY_TAG);
		_defaultTagOrder.add(MODIFICATION_DATE_TAG);
	}
}
