package org.obo.nlp.impl;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.logging.Logger;

import org.bbop.util.StringUtil;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.ObsoletableObject;
import org.obo.datamodel.impl.DefaultOperationModel;
import org.obo.history.HistoryItem;
import org.obo.history.TermMacroHistoryItem;
import org.obo.nlp.SemanticParser;
import org.obo.util.TermUtil;

/**
 * 
 * @author cjm
 *
 * TODO: remove part_ofs where regulates relation exists
 * TODO: do not replace existing xps
 * TODO: add relations to ontology if they do not exist
 * TODO: different status if NP but has a parsed parent
 * TODO: realize links
 */
public class RegulationTermParser implements SemanticParser {

	protected Map<String,LinkedObject>name2obj = 
		new HashMap<String,LinkedObject>();
	protected Collection<String> reports = new LinkedList<String>();
	protected OBOSession session;
	Logger logger = Logger.getLogger("org.obo.nlp");

	LinkedObject brObj ;
	LinkedObject romfObj ;
	LinkedObject robqObj;
	LinkedObject robpObj ;
	LinkedObject mfObj ;
	LinkedObject bpObj ;
	
	OBOProperty regRel;
	OBOProperty negRegRel;
	OBOProperty posRegRel;
	String regRelId;
	String negRegRelId;
	String posRegRelId;
	


	public RegulationTermParser() {
		super();
	}



	public Collection<String> getReports() {
		return reports;
	}


	public void index(OBOSession session) {

		name2obj = new HashMap<String,LinkedObject>();

		for (IdentifiedObject io : session.getObjects()) {
			if (io.isBuiltIn())
				continue;
			if (io instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject)io;
				for (String label : TermUtil.getLabels(lo)) {
					label = label.replace('_', ' ');
					logger.info("label="+label);
					name2obj.put(label, lo);
				}
			}
		}
		this.session = session;
	}

	public Collection<TermMacroHistoryItem> parseTerms() {
		Collection<TermMacroHistoryItem> items =
			new LinkedList<TermMacroHistoryItem>();
		brObj = lookup("biological regulation");
		romfObj = lookup("regulation of a molecular function");
		robqObj= lookup("regulation of biological quality");
		robpObj = lookup("regulation of biological process");
		mfObj = lookup("molecular_function");
		bpObj = lookup("biological_process");
		
		regRel = (OBOProperty)session.getObject(regRelId);
		
		for (IdentifiedObject io : session.getObjects()) {
			if (io.isBuiltIn())
				continue;
			if (io instanceof LinkedObject) {
				items.addAll(parseTerm((LinkedObject)io));
			}
		}
		return items;
	}	

	public Collection<TermMacroHistoryItem> parseTerm(LinkedObject lo) {
		if (lo.isBuiltIn() || ( lo instanceof ObsoletableObject && ((ObsoletableObject)lo).isObsolete()) ) {
			return Collections.EMPTY_LIST;			
		}
		TermMacroHistoryItem item = parseTerm(lo,lo.getName());
		if (item == null) { // ro
			if (TermUtil.hasIsAAncestor(lo, brObj)) {
				report("NP",lo,lo.getName(),"No parse for regulation subclass");
			}
			// TODO: synonyms
			return Collections.EMPTY_LIST;
		}
		else {
			return Collections.singletonList(item);
		}
	}

	public TermMacroHistoryItem parseTerm(LinkedObject lo, String name) {
		if (name == null)
			return null;
		name = name.replace('_', ' ');
//		logger.info("splitting: "+name+" in:"+lo);
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
				report("NO_TARGET",lo,name,"cannot find regulated entity term");
				logger.info("no target: "+targetName);
			}
			else {
				String relationName;
				// we use specific relations since we don't have + and -
				// genus terms for RoQ and RoMF
				if (dir.equals("positive"))
					relationName = "positively_regulates";
				else if (dir.equals("negative"))
					relationName = "negatively_regulates";
				else
					relationName = "regulates";
				LinkedObject genus = lookup("biological regulation");
				if (TermUtil.hasIsAAncestor(lo,robpObj))
					if (TermUtil.hasIsAAncestor(target, bpObj))
						report("OK", lo, name, "in correct place");
					else
						report("HIERARCHY",lo,name,"should be in BP",target);

				if (TermUtil.hasIsAAncestor(lo,romfObj))
					if (TermUtil.hasIsAAncestor(target, mfObj))
						report("OK", lo, name, "in correct place");
					else
						report("HIERARCHY",lo,name,"should be in MF",target);
				if (TermUtil.hasIsAAncestor(lo,robqObj))
					report("UNEXPECTED",lo,name,"biological quality found in GO",target);

				if (!TermUtil.hasIsAAncestor(lo,brObj))
					report("MISSING_LINK",lo,name,"not under biological regulation, yet I can parse it");

				String relID = relationName;
				TermMacroHistoryItem item =
					TermUtil.createGenusDifferentiaHistoryItem(lo, genus, relID, target);
				return item;
			}
		}
		return null;

	}

	public void report(String categ, LinkedObject lo, String name, String message) {
		report(categ,lo,name,message,null);
	}
	public void report(String categ, LinkedObject lo, String name, String message, LinkedObject target) {
		StringBuffer msg = new StringBuffer();
		msg.append(categ + ": "+lo.getID()+" \""+name+"\" MSG: "+message);
		if (target != null)
			msg.append(" TARGET: "+target);
		reports.add(msg.toString());
	}

	public LinkedObject lookup(String n) {
		n = n.replace('_', ' ');
		return name2obj.get(n);
	}

	public void apply(Collection<TermMacroHistoryItem> items) {
		DefaultOperationModel model = new DefaultOperationModel();
		model.setSession(session);
		for (HistoryItem item : items) {
			model.apply(item);
		}
	}


}
