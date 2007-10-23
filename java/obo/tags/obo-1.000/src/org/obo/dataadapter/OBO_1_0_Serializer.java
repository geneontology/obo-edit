package org.obo.dataadapter;

import java.io.*;
import java.util.*;

import org.obo.datamodel.*;

public class OBO_1_0_Serializer extends OBO_1_2_Serializer {

	protected Comparator<Synonym> synonymComparator = new Comparator<Synonym>() {
		public int compare(Synonym sa, Synonym sb) {
			int compVal = sa.getScope() - sb.getScope();
			if (compVal == 0) {
				return Synonym.COMPARATOR.compare(sa, sb);
			} else
				return compVal;
		}
	};

	@Override
	public Comparator getSynonymComparator() {
		return synonymComparator;
	}

	@Override
	public void writeFormatVersionHeaderTag() throws IOException {
		println("format-version: 1.0");
	}

	@Override
	public void writeDataVersionHeaderTag(String dataVersion) throws IOException {
		println("version: "+dataVersion);
	}
	
	@Override
	public void writeSynonymTypeDefHeaderTag(SynonymCategory category)
			throws IOException {
		print("!synonymtypedef: " + escapeBeforeQuotes(category.getID())
				+ " \"" + escapeQuoted(category.getName()) + "\"");
		if (category.getScope() != Synonym.UNKNOWN_SCOPE)
			print(" " + getScopeStr(category.getScope()));
		print(" ! synonymtypedef not supported by OBO 1.0");
		println();
	}

	@Override
	public void writeSynonymTag(Synonym syn, NestedValue nv) throws IOException {
		if (syn.getScope() == Synonym.EXACT_SYNONYM)
			print("exact_synonym: ");
		else if (syn.getScope() == Synonym.NARROW_SYNONYM)
			print("narrow_synonym: ");
		else if (syn.getScope() == Synonym.BROAD_SYNONYM)
			print("broad_synonym: ");
		else
			print("related_synonym: ");
		print("\"" + escapeQuoted(syn.getText()) + "\"");
		writeDbxrefList(syn.getDbxrefs());
		writeNestedValue(nv);
		if (syn.getSynonymCategory() != null)
			print(" ! belonged to category " + syn.getSynonymCategory().getID());
		println();
	}

	@Override
	public void writeXrefTag(Dbxref ref) throws IOException {
		print("xref_analog: ");
		writeDbxref(ref);
		println();
	}

	@Override
	public String getID() {
		return "OBO 1.0";
	}
}
