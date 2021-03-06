package org.obo.dataadapter;

import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.obo.util.TermUtil;

import java.text.*;
import java.util.*;
import java.io.*;

import org.apache.log4j.*;

public class OBO_1_2_Serializer implements OBOSerializer {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OBO_1_2_Serializer.class);

	protected PrintStream stream;

	LinkedList serializer_1_2_tagOrdering = new LinkedList();

	public LinkedList getSerializer_1_2_tagOrdering() {
		return serializer_1_2_tagOrdering;
	}

	public void setSerializer_1_2_tagOrdering(LinkedList serializer_1_2_tagOrdering) {
		this.serializer_1_2_tagOrdering = serializer_1_2_tagOrdering;
	}

	HashSet<OBOConstants.TagMapping> tagsToWrite;
	
	protected List<PropertyValue> scratch = new ArrayList<PropertyValue>();

	protected List<PropertyValue> writeNestedValueScratch = new ArrayList<PropertyValue>();

	protected Namespace defaultNamespace = new Namespace("<default namespace>");
	protected IdentifiedObject currentObject;

	protected OBOSerializationEngine engine;

	protected SimpleDateFormat oldDateFormat = new SimpleDateFormat("dd:MM:yyyy HH:mm");
	// Using 'Z' for timezone because ISO 8601 requires the timezone to be 'Z' or +HH:mm or -HH:mm
	// and SimpleDateFormat can only handle timezones of the form hhmm
	protected SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'"); // ISO 8601

	public void setOutputStream(PrintStream stream) throws IOException {
		this.stream = stream;
	}

	public void setEngine(OBOSerializationEngine engine) {
		this.engine = engine;
	}

	public String mapID(IdentifiedObject io, String id) {
		return null;
	}

	public void startSerialize() throws IOException {
	}

	public void endSerialize() throws IOException {
	}

	public List getHeaderTagOrdering() {
		return null;
	}

	public List getStanzaOrdering() {
		return null;
	}

	public List getTagOrdering() {
		getSerializer_1_2_tagOrdering();
		return serializer_1_2_tagOrdering;
	}

	//This is where the tagOrdering is set.
	//This method is called in AdvancedOBOUI.java in
	//the store method. This means that by the time we get back to commit() in GraphicalAdapterChooser, the tag
	//ordering is set and can be accessed using getTagsToWrite(). 
	
	public void setTagOrdering(HashSet tagsToWrite) {
//		logger.debug("OBO_1_2_Serializer: setTagOrdering: tagsToWrite = " + tagsToWrite);

		if(tagsToWrite != null){
		serializer_1_2_tagOrdering = getSerializer_1_2_tagOrdering();
		serializer_1_2_tagOrdering.clear();
		//This gets the order right.
		serializer_1_2_tagOrdering.addAll(OBOConstants.DEFAULT_TAG_ORDER);
		//This keeps only the tags that the user wants. 
		serializer_1_2_tagOrdering.retainAll(tagsToWrite);
		setSerializer_1_2_tagOrdering(serializer_1_2_tagOrdering);
//		logger.debug("OBO_1_2_Serializer: setTagOrdering: serializer_1_2_tagOrdering = " + getSerializer_1_2_tagOrdering());
		}
		
		return;
		
	}

	public Comparator getObjectComparator() {
		return null;
	}

	public Comparator getIDComparator() {
		return null;
	}

	public Comparator getDbxrefComparator() {
		return null;
	}

	public Comparator getSynonymComparator() {
		return null;
	}

	public Comparator getSynonymCategoryComparator() {
		return null;
	}

	public Comparator getCategoryComparator() {
		return null;
	}

	public Comparator getXrefComparator() {
		return null;
	}

	public Comparator getObsoleteComparator() {
		return null;
	}

	public Comparator getLinkComparator() {
		return null;
	}

	public static String escapeSpaces(String s) {
		return OBOSerializationEngine.escapeSpaces(s);
	}

	public static String escapeQuoted(String s) {
		return OBOSerializationEngine.escapeQuoted(s, '"');
	}

	public static String escapeQuoted(String s, char c) {
		return OBOSerializationEngine.escapeQuoted(s, c);
	}

	public static String escapeBeforeQuotes(String s) {
		return OBOSerializationEngine.escapeBeforeQuotes(s);
	}

	public static String escapePVName(String s) {
		return OBOSerializationEngine.escapePVName(s);
	}

	public static String escapePVValue(String s) {
		return "\""+escapeQuoted(s)+"\"";
		//return OBOSerializationEngine.escapePVValue(s);
	}

	public static String escapeDbxref(String s) {
		return OBOSerializationEngine.escapeDbxref(s);
	}

	public static String escape(String s) {
		return OBOSerializationEngine.escape(s);
	}

	public static String escapeBlocktext(String s) {
		return OBOSerializationEngine.escapeBlocktext(s);
	}

	/*
	 * public void write public void writeDbxrefObject(Dbxref ref, int nesting);
	 */
	public void startHeader() throws IOException {
	}

	public void endHeader() throws IOException {
		println();
	}

	public void writeFormatVersionHeaderTag() throws IOException {
		print("format-version: 1.2\n");
	}

	public void writeDataVersionHeaderTag(String dataVersion)
	throws IOException {
		print("data-version: " + dataVersion + "\n");
	}

	/** For now, use old date format for date field in header so that it doesn't choke older versions of OBO-Edit */
	public void writeDateHeaderTag(Date date) throws IOException {
		print("date: " + oldDateFormat.format(date) + "\n");
	}

	public void writeSavedByHeaderTag(String user) throws IOException {
		print("saved-by: " + user + "\n");
	}

	public void writeAutoGeneratedByHeaderTag(String generatedBy)
	throws IOException {
		println("auto-generated-by: " + generatedBy);
	}

	public void writeSubsetDefHeaderTag(TermSubset category)
	throws IOException {
		print("subsetdef: " + escapeBeforeQuotes(category.getName()) + " \""
				+ escapeQuoted(category.getDesc(), '"') + "\"\n");
	}

	public void writeNamespaceIDRuleHeaderTag(String ns, String rule)
	throws IOException {
		String nsName = "*";
		if (ns != null)
			nsName = ns;
		print("namespace-id-rule: " + escapeSpaces(nsName) + " " + rule + "\n");
	}

	public void writeSynonymTypeDefHeaderTag(SynonymType category)
	throws IOException {
		print("synonymtypedef: " + escapeBeforeQuotes(category.getID()) + " \""
				+ escapeQuoted(category.getName()) + "\"");
		if (category.getScope() != Synonym.UNKNOWN_SCOPE)
			print(" " + getScopeStr(category.getScope()));
		println();
	}

	public void writeDefaultNamespaceHeaderTag(Namespace namespace)
	throws IOException {
		if (namespace != null) {
			defaultNamespace = namespace;
			print("default-namespace: " + escape(namespace.getID()) + "\n");
		}
	}

	public void writeRemarkHeaderTag(String remark) throws IOException {
		if (remark == null)
			return;
		StringTokenizer tokenizer = new StringTokenizer(remark, "\n");
		while (tokenizer.hasMoreTokens()) {
			String remarkLine = tokenizer.nextToken();
			print("remark: " + remarkLine);
			println();
		}
	}

	public void startStanza(IdentifiedObject obj) throws IOException {
		if (TermUtil.isClass(obj))
			print("[Term]\n");
		else if (TermUtil.isProperty(obj))
			print("[Typedef]\n");
		else if (TermUtil.isInstance(obj))
			print("[Instance]\n");
		else
			print("[" + mapID(obj.getType(), obj.getType().getID()) + "]\n");
		currentObject = obj;
	}

	public void endStanza(IdentifiedObject obj) throws IOException {
		println();
		currentObject = null;
	}

	public void writeIDTag(String id, NestedValue nv) throws IOException {
		print("id: " + id);
		writeNestedValue(nv);
		println();
	}

	public void writeIsAnonymousTag(boolean value, NestedValue nv)
	throws IOException {
		if (value || nv != null) {
			print("is_anonymous: " + value);
			writeNestedValue(nv);
			println();
		}
	}

	public void writeIsMetadataTag(boolean value, NestedValue nv)
	throws IOException {
		if (value || nv != null) {
			print("is_metadata_tag: " + value);
			writeNestedValue(nv);
			println();
		}
	}

	public void writeNameTag(String name, NestedValue nv) throws IOException {
		if (name != null ) {
			print("name: " + escape(name));
			writeNestedValue(nv);
			println();
		}
	}

	public void writeNamespaceTag(Namespace ns, NestedValue nv)
	throws IOException {
		if (nv == null) {
			if (ns == null || ns.equals(defaultNamespace))
				return;
		}
		if (ns == null)
			ns = defaultNamespace;

		print("namespace: " + ns.getID());
		writeNestedValue(nv);
		println();
	}

	public void writeAltIDTag(String id, NestedValue nv) throws IOException {
		print("alt_id: " + id);
		writeNestedValue(nv);
		println();
	}

	public void writeDefTag(String def, Collection dbxrefs, boolean includeDesc, NestedValue nv)
	throws IOException {
		if (def.length() > 0 || nv != null) {
			print("def: \"" + escapeQuoted(def) + "\"");
			writeDbxrefList(dbxrefs, includeDesc);
			writeNestedValue(nv);
			println();
		}
	}

	public void writeCommentTag(String comment, NestedValue nv)
	throws IOException {
		if ((comment != null && comment.length() > 0) || nv != null) {
			print("comment: " + escapeBlocktext(comment));
			writeNestedValue(nv);
			println();
		}
	}

	public void writeSubsetTag(TermSubset category, NestedValue nv)
	throws IOException {
		print("subset: " + category.getName());
		writeNestedValue(nv);
		println();
	}

	public void writeSynonymTag(Synonym syn, boolean includeDesc, NestedValue nv) throws IOException {
		print("synonym: \"" + escapeQuoted(syn.getText()) + "\"");
		if (syn.getScope() != Synonym.RELATED_SYNONYM
				|| syn.getScope() != Synonym.UNKNOWN_SCOPE
				|| syn.getSynonymType() != null) {
			print(" " + getScopeStr(syn.getScope()));
			if (syn.getSynonymType() != null) {
				print(" " + syn.getSynonymType().getID());
			}
		}
		List<Dbxref> dbxrefs = new ArrayList<Dbxref>(syn.getXrefs());
		Comparator c = getDbxrefComparator();
		if (c == null)
			c = OBOConstants.DEFAULT_DBXREF_COMPARATOR;
		Collections.sort(dbxrefs, c);
		writeDbxrefList(dbxrefs, includeDesc);
		writeNestedValue(nv);
		println();
	}

	public void writeXrefTag(Dbxref ref) throws IOException {
		print("xref: ");
		writeDbxref(ref, true); // always write xref descriptions, see geneontology-Bugs-3366931
		println();
	}

	public void writeInstanceOfTag(Type type, NestedValue nv)
	throws IOException {
		if (type != null) {
			print("instance_of: " + engine.mapID(type, type.getID()));
			writeNestedValue(nv);
			println();
		}
	}

	public void writePropertyValueTag(PropertyValue pv, NestedValue nv,
			int depth) throws IOException {
		throw new UnsupportedOperationException("can't write operations");
	}

	public void writeDomainTag(IdentifiedObject domain, NestedValue nv)
	throws IOException {
		print("domain: " + engine.mapID(domain, domain.getID()));
		writeNestedValue(nv);
		if (domain.getName() != null)
			print(" ! " + domain.getName());
		println();
	}

	public void writeHoldsOverChainTag(List<OBOProperty> chain) throws IOException {
		print("holds_over_chain:");
		for (OBOProperty prop : chain) {
			print(" " + engine.mapID(prop, prop.getID()));
		}
		//print(" ! " + domain.getName());
		println();
	}


	public void writeRangeTag(Type range, NestedValue nv) throws IOException {
		print("range: " + engine.mapID(range, range.getID()));
		writeNestedValue(nv);
		if (range.getName() != null)
			print(" ! " + range.getName());
		println();
	}

	public void writeIsCyclicTag(boolean value, NestedValue nv)
	throws IOException {
		if (!value)
			return;
		print("is_cyclic: " + value);
		writeNestedValue(nv);
		println();
	}

	public void writeIsReflexiveTag(boolean value, NestedValue nv)
	throws IOException {
		if (!value)
			return;
		print("is_reflexive: " + value);
		writeNestedValue(nv);
		println();
	}

	public void writeAlwaysImpliesInverseTag(boolean value, NestedValue nv)
	throws IOException {
		if (!value)
			return;
		print("always_implies_inverse: " + value);
		writeNestedValue(nv);
		println();
	}

	public void writeIsSymmetricTag(boolean value, NestedValue nv)
	throws IOException {
		if (!value)
			return;
		print("is_symmetric: " + value);
		writeNestedValue(nv);
		println();
	}

	public void writeIsTransitiveTag(boolean value, NestedValue nv)
	throws IOException {
		if (!value)
			return;
		print("is_transitive: " + value);
		writeNestedValue(nv);
		println();
	}

	public void writeValueLinkTag(ValueLink link, NestedValue nv)
	throws IOException {

		if (link.getValue() instanceof DatatypeValue) {
			DatatypeValue val = (DatatypeValue) link.getValue();
			stream.print("property_value: " + engine.mapID(link.getType())
					+ " \"" + escapeQuoted(val.getValue()) + "\" "
					+ engine.mapID(val.getType())+"\n");
		} else if (link.getParent() != null) {
			stream.print("property_value: "+engine.mapID(link.getType()) + " " +
					engine.mapID(link.getParent())+"\n");
			//writeLinkTag(link, nv);
		} else
			throw new RuntimeException("Unexpected valuelink value from "+link);
	}

	public void writeLinkTag(Link link, NestedValue nv) throws IOException {
		scratch.clear();
		if (TermUtil.isIntersection(link)) {
			stream.print("intersection_of: ");
			if (!link.getType().equals(OBOProperty.IS_A))
				stream.print(engine.mapID(link.getType(), link.getType()
						.getID())
						+ " ");
		} else if (link.getType().equals(OBOProperty.IS_A)) {
			stream.print("is_a: ");
		} else if (link.getType().equals(OBOProperty.DISJOINT_FROM)) {
			stream.print("disjoint_from: ");
		} else if (link.getType().equals(OBOProperty.TRANSITIVE_OVER)) {
			stream.print("transitive_over: ");
		} else if (link.getType().equals(OBOProperty.DISJOINT_OVER)) {
			stream.print("disjoint_over: ");
		} else if (link.getType().equals(OBOProperty.UNION_OF)) {
			stream.print("union_of: ");
		} else if (link.getType().equals(OBOProperty.INVERSE_OF)) {
			stream.print("inverse_of: ");
		} else {
			if (link instanceof OBORestriction) {
				OBORestriction res = (OBORestriction) link;
				if (!res.isNecessarilyTrue())
					scratch.add(new PropertyValueImpl("necessary", "false"));
				if (res.isInverseNecessarilyTrue())
					scratch.add(new PropertyValueImpl("inverse_necessary",
					"true"));
				if (res.getCardinality() != null)
					scratch.add(new PropertyValueImpl("cardinality", res
							.getCardinality().toString()));
				if (res.getMaxCardinality() != null)
					scratch.add(new PropertyValueImpl("maxCardinality", res
							.getMaxCardinality().toString()));
				if (res.getMinCardinality() != null)
					scratch.add(new PropertyValueImpl("minCardinality", res
							.getMinCardinality().toString()));
			}
			stream.print("relationship: "
					+ engine.mapID(link.getType(), link.getType().getID())
					+ " ");
		}

		if (link.getNamespace() != null
				&& !link.getNamespace().equals(link.getChild().getNamespace()))
			scratch.add(new PropertyValueImpl("namespace", link.getNamespace()
					.toString()));
		if (TermUtil.isImplied(link))
			scratch.add(new PropertyValueImpl("implied", "true"));

		if (link.getParent() == null) {
			logger.error("invalid link object: "+link);	
		}
		stream.print(engine.mapID(link.getParent(), link.getParent().getID()));
		if (link instanceof OBORestriction) {
			OBORestriction r = (OBORestriction) link;
			if (r.getNumberOfAdditionalArguments() > 0) {
				for (LinkedObject a : r.getAdditionalArguments()) {
					stream.print(" "+engine.mapID(a, a.getID()));					
				}
			}
		}
		writeNestedValue(nv, scratch);
		if (link.getParent().getName() != null)
			print(" ! " + link.getParent().getName());
		println();
	}

	public void writeIsObsoleteTag(boolean value, NestedValue nv)
	throws IOException {

		if (!value)
			return;
		print("is_obsolete: " + value);
		writeNestedValue(nv);
		println();
	}

	public void writeReplacedByTag(ObsoletableObject replacedBy, NestedValue nv)
	throws IOException {
		print("replaced_by: " + engine.mapID(replacedBy, replacedBy.getID()));
		writeNestedValue(nv);
		println();
	}

	public void writeConsiderTag(ObsoletableObject consider, NestedValue nv)
	throws IOException {
		print("consider: " + engine.mapID(consider, consider.getID()));
		writeNestedValue(nv);
		println();
	}

	public void writeUnrecognizedTag(PropertyValue pv) throws IOException {
		println(pv.getProperty() + ": " + escape(pv.getValue()));
	}

	protected void print(String s) throws IOException {
		stream.print(s);
	}

	protected void println(String s) throws IOException {
		stream.print(s + "\n");
	}

	protected void println() throws IOException {
		stream.print("\n");
	}

	/*
	 * protected static String escape(String string) { return
	 * GOBOParseEngine.escape(string, false); }
	 */
	protected static String getScopeStr(int scope) {
		if (scope == Synonym.UNKNOWN_SCOPE || scope == Synonym.RELATED_SYNONYM)
			return "RELATED";
		else if (scope == Synonym.EXACT_SYNONYM)
			return "EXACT";
		else if (scope == Synonym.BROAD_SYNONYM)
			return "BROAD";
		else if (scope == Synonym.NARROW_SYNONYM)
			return "NARROW";
		else
			return null;
	}

	protected void writeDbxrefList(Collection dbxrefs, boolean includeDesc) throws IOException {
		print(" [");
		Iterator it = dbxrefs.iterator();
		boolean first = true;
		while (it.hasNext()) {
			Dbxref dbxref = (Dbxref) it.next();
			if (!first)
				print(", ");
			writeDbxref(dbxref, includeDesc);
			first = false;
		}
		print("]");
	}

    protected void writeDbxref(Dbxref dbxref, boolean includeDesc) throws IOException {
		print(escapeDbxref(dbxref.getDatabase()));
		print(":");
		print(escapeDbxref(dbxref.getDatabaseID()));
                if (includeDesc) {
                    if (dbxref.getDesc() != null && dbxref.getDesc().length() > 0) {
                        print(" \"" + escapeQuoted(dbxref.getDesc()) + "\"");
                    }
                }
		writeNestedValue(dbxref.getNestedValue());
	}

	protected void writeNestedValue(NestedValue nv) throws IOException {
		writeNestedValue(nv, null);
	}

	protected void writeNestedValue(NestedValue nv, List additionalVals)
	throws IOException {
		boolean notFirst = false;
		writeNestedValueScratch.clear();
		if (nv != null)
			writeNestedValueScratch.addAll(nv.getPropertyValues());
		if (additionalVals != null)
			writeNestedValueScratch.addAll(additionalVals);

		if (writeNestedValueScratch.size() == 0
				&& (nv == null || nv.getSuggestedComment() == null))
			return;

		if (writeNestedValueScratch.size() > 0) {
			Collections.sort(writeNestedValueScratch, PropertyValue.COMPARATOR);

			Iterator it = writeNestedValueScratch.iterator();

			print(" {");
			for (PropertyValue pv : writeNestedValueScratch) {
				if (notFirst)
					print(", ");
				print(escapePVName(pv.getProperty()) + "="
						+ escapePVValue(pv.getValue()));
				notFirst = true;
			}

			print("}");
		}
		if (nv != null && nv.getSuggestedComment() != null)
			print(" ! " + nv.getSuggestedComment());
	}

	public String getID() {
		return "OBO 1.2";
	}

	@Override
	public String toString() {
		return getID();
	}

	public void writeCreatedByTag(String createdBy, NestedValue nv) throws IOException {
		print("created_by: " + escape(createdBy));
		writeNestedValue(nv);
		println();
	}

	// OE1 will ignore creation_date tags.  However, note that using the new date format
	// for creation_date tags will choke older OE2 betas (which are expecting the old
	// date format.)
	public void writeCreationDateTag(Date date, NestedValue nv) throws IOException {
		print("creation_date: "+dateFormat.format(date));
		writeNestedValue(nv);
		println();
	}

	public void writeModificationDateTag(Date date, NestedValue nv) throws IOException {
		print("modification_date: "+dateFormat.format(date));
		writeNestedValue(nv);
		println();
	}

	public void writeModifiedByTag(String modifiedBy, NestedValue nv) throws IOException {
		print("modified_by: "+escape(modifiedBy));
		writeNestedValue(nv);
		println();
	}

	public void writeIDSpaceHeaderTag(String idspace, String uriPrefix) throws IOException {
		print("idspace: " + escapeBeforeQuotes(idspace) + " "+uriPrefix);
		println();		
	}

	public void writeGenericHeaderTag(String property, String value) throws IOException {
		print(property+": "+value);
		println();

	}


}
