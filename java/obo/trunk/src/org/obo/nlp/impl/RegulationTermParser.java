package org.obo.nlp.impl;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;

import org.bbop.util.StringUtil;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBOSession;
import org.obo.history.TermMacroHistoryItem;
import org.obo.nlp.Namer;
import org.obo.nlp.SemanticParser;
import org.obo.util.ReasonerUtil;
import org.obo.util.TermUtil;

public class RegulationTermParser implements SemanticParser {
	
	protected Map<String,LinkedObject>name2obj = 
		new HashMap<String,LinkedObject>();
	protected Collection<String> reports = new LinkedList<String>();
	protected OBOSession session;

	public void index(OBOSession session) {
		
		name2obj = new HashMap<String,LinkedObject>();
		
		for (IdentifiedObject io : session.getObjects()) {
			if (io instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject)io;
				for (String label : TermUtil.getLabels(lo)) {
					name2obj.put(label, lo);
				}
			}
		}
		this.session = session;
	}
	
	public Collection<TermMacroHistoryItem> parseTerms() {
		Collection<TermMacroHistoryItem> items =
			new LinkedList<TermMacroHistoryItem>();
		LinkedObject romf = lookup("regulation of a molecular function");
		
		return items;
	}	
	
	public Collection<TermMacroHistoryItem> parseTerm(LinkedObject lo) {
		TermMacroHistoryItem item = parseTerm(lo,lo.getName());
		if (item == null) {
			reports.add("No parse for "+lo);
			// TODO: synonyms
			return Collections.EMPTY_LIST;
		}
		else {
			return Collections.singletonList(item);
		}
	}
	
	public TermMacroHistoryItem parseTerm(LinkedObject lo, String name) {
		String[] tokens = name.split("\\s");
		int p = 0;
		String dir="";
		if (tokens[0].equals("positive") || tokens[0].equals("negative")) {
			p++;
			dir = tokens[0];
		}
		if (tokens[p].equals("regulation") && tokens[p+1].equals("of")) {
			p=p+2;
			String targetName = StringUtil.join(" ",tokens,p);
			LinkedObject target = lookup(targetName);
			if (target == null) {
			}
			else {
				String relationName;
				if (dir.equals("positive"))
					relationName = "positively_regulates";
				if (dir.equals("negative"))
					relationName = "negatively_regulates";
				else
					relationName = "regulates";
				LinkedObject genus = lookup("biological regulation");
				String relID = relationName;
				TermMacroHistoryItem item =
					TermUtil.createGenusDifferentiaHistoryItem(lo, genus, relID, target);
				return item;
			}
		}
		return null;

	}
	
	public LinkedObject lookup(String n) {
		return name2obj.get(n);
	}


}
