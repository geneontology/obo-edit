package org.obo.dataadapter;

import org.obo.datamodel.*;

import java.util.*;
import java.io.*;

public interface OBOSerializer {

	public void setEngine(OBOSerializationEngine engine);

	public void setOutputStream(PrintStream stream) throws IOException;

	public void startSerialize() throws IOException;

	public void endSerialize() throws IOException;

	public String mapID(IdentifiedObject io, String id);

	public List getHeaderTagOrdering();

	public List getStanzaOrdering();

	public List getTagOrdering();

	public Comparator getObjectComparator();

	public Comparator getIDComparator();

	public Comparator getDbxrefComparator();

	public Comparator getSynonymComparator();

	public Comparator getSynonymCategoryComparator();

	public Comparator getCategoryComparator();

	public Comparator getXrefComparator();

	public Comparator getObsoleteComparator();

	public Comparator getLinkComparator();

	/*
	 * public void write public void writeDbxrefObject(Dbxref ref, int nesting);
	 */
	public void startHeader() throws IOException;

	public void endHeader() throws IOException;

	public void writeFormatVersionHeaderTag() throws IOException;

	public void writeDataVersionHeaderTag(String dataVersion)
			throws IOException;

	public void writeDateHeaderTag(Date date) throws IOException;

	public void writeSavedByHeaderTag(String user) throws IOException;

	public void writeAutoGeneratedByHeaderTag(String generatedBy)
			throws IOException;

	public void writeSubsetDefHeaderTag(TermCategory category)
			throws IOException;

	public void writeSynonymTypeDefHeaderTag(SynonymCategory category)
			throws IOException;

	public void writeIDSpaceHeaderTag(String idspace, String uriPrefix)
			throws IOException;

	public void writeDefaultNamespaceHeaderTag(Namespace namespace)
			throws IOException;

	public void writeRemarkHeaderTag(String remark) throws IOException;

	public void writeNamespaceIDRuleHeaderTag(String ns, String rule)
			throws IOException;

	public void startStanza(IdentifiedObject obj) throws IOException;

	public void endStanza(IdentifiedObject obj) throws IOException;

	public void writeIDTag(String id, NestedValue nv) throws IOException;

	public void writeIsAnonymousTag(boolean value, NestedValue nv)
			throws IOException;

	public void writeNameTag(String name, NestedValue nv) throws IOException;

	public void writeNamespaceTag(Namespace ns, NestedValue nv)
			throws IOException;

	public void writeAltIDTag(String id, NestedValue nv) throws IOException;

	public void writeDefTag(String def, Collection dbxrefs, NestedValue nv)
			throws IOException;

	public void writeCommentTag(String comment, NestedValue nv)
			throws IOException;

	public void writeSubsetTag(TermCategory category, NestedValue nv)
			throws IOException;

	public void writeSynonymTag(Synonym synonym, NestedValue nv)
			throws IOException;

	public void writeXrefTag(Dbxref ref) throws IOException;

	public void writeInstanceOfTag(Type type, NestedValue nv)
			throws IOException;

	public void writePropertyValueTag(PropertyValue pv, NestedValue nv,
			int depth) throws IOException;

	public void writeDomainTag(IdentifiedObject domain, NestedValue nv)
			throws IOException;

	public void writeRangeTag(Type range, NestedValue nv) throws IOException;

	public void writeIsCyclicTag(boolean value, NestedValue nv)
			throws IOException;
	
	public void writeAlwaysImpliesInverseTag(boolean value, NestedValue nv)
		throws IOException;

	public void writeIsReflexiveTag(boolean value, NestedValue nv)
			throws IOException;

	public void writeIsSymmetricTag(boolean value, NestedValue nv)
			throws IOException;

	public void writeIsTransitiveTag(boolean value, NestedValue nv)
			throws IOException;

	public void writeValueLinkTag(ValueLink link, NestedValue nv)
			throws IOException;

	public void writeLinkTag(Link link, NestedValue nv) throws IOException;

	public void writeIsObsoleteTag(boolean value, NestedValue nv)
			throws IOException;

	public void writeReplacedByTag(ObsoletableObject replacedBy, NestedValue nv)
			throws IOException;

	public void writeConsiderTag(ObsoletableObject consider, NestedValue nv)
			throws IOException;

	public void writeUnrecognizedTag(PropertyValue pv) throws IOException;

	public String getID();

	public void writeCreatedByTag(String createdBy,
			NestedValue createdByExtension) throws IOException;

	public void writeModifiedByTag(String modifiedBy,
			NestedValue createdByExtension) throws IOException;

	public void writeCreationDateTag(Date date, NestedValue createdByExtension)
			throws IOException;

	public void writeModificationDateTag(Date date,
			NestedValue createdByExtension) throws IOException;

	public void writeHoldsOverChainTag(List<OBOProperty> chain) throws IOException;
}
