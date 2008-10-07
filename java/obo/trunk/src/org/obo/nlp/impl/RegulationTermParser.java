package org.obo.nlp.impl;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.logging.Logger;

import org.bbop.util.StringUtil;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBORestriction;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.ObsoletableObject;
import org.obo.datamodel.impl.DefaultOperationModel;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.history.DeleteLinkHistoryItem;
import org.obo.history.HistoryItem;
import org.obo.history.TermMacroHistoryItem;
import org.obo.nlp.Namer;
import org.obo.nlp.SemanticParser;
import org.obo.util.ReasonerUtil;
import org.obo.util.TermUtil;

/**
 * 
 * This parser implements the rule
 * 
 *   process --> Direction? regulation of ?Process
 *   Direction --> negative ; positive
 *   
 * See also: http://www.berkeleybop.org/obol
 * 
 * @author cjm
 *
 * TODO: use some grammar. Direct conversion of prolog rules
 * TODO: remove part_ofs where regulates relation exists
 * TODO: do not replace existing xps
 * TODO: add relations to ontology if they do not exist
 * TODO: different status if NP but has a parsed parent
 * TODO: realize links
 */
import org.apache.log4j.*;

public class RegulationTermParser implements SemanticParser {

	//initialize logger
	protected final static Logger logger = Logger.getLogger("RegulationTermParser.class");

	protected Map<String,LinkedObject>name2obj = 
		new HashMap<String,LinkedObject>();
	protected Collection<String> reports = new LinkedList<String>();
	protected OBOSession session;


	boolean replacePartOfs = true;

	LinkedObject brObj ;
	LinkedObject romfObj ;
	LinkedObject robqObj;
	LinkedObject robpObj ;
	LinkedObject mfObj ;
	LinkedObject bpObj ;

	OBOProperty regRel;	
	OBOProperty partOfRel;
	OBOProperty negRegRel;
	OBOProperty posRegRel;
	String regRelId = "regulates";
	String negRegRelId = "negatively_regulates";
	String posRegRelId = "positively_regulates";



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
				for (String label : TermUtil.getExactLabels(lo)) {
					label = label.replace('_', ' ');
					//					logger.info("label="+label);
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
		romfObj = lookup("regulation of molecular function");
		robqObj= lookup("regulation of biological quality");
		robpObj = lookup("regulation of biological process");
		mfObj = lookup("molecular_function");
		bpObj = lookup("biological_process");

		partOfRel = (OBOProperty)session.getObject("part_of");

		// add relations if not present
		regRel = (OBOProperty)session.getObject(regRelId);
		if (regRel == null) {
			regRel = (OBOProperty)
			session.getObjectFactory().createObject(regRelId, 
					OBOClass.OBO_PROPERTY, false);
			session.addObject(regRel);
			regRel.setTransitiveOver(partOfRel);
		}
		posRegRel = (OBOProperty)session.getObject(posRegRelId);
		if (posRegRel == null) {
			posRegRel = (OBOProperty)
			session.getObjectFactory().createObject(posRegRelId, 
					OBOClass.OBO_PROPERTY, false);
			session.addObject(posRegRel);
			Link subProp = 
				new OBORestrictionImpl(posRegRel,OBOProperty.IS_A,
						regRel, false);
			posRegRel.addParent(subProp);
			posRegRel.setTransitiveOver(partOfRel);
		}
		negRegRel = (OBOProperty)session.getObject(negRegRelId);
		if (negRegRel == null) {
			negRegRel = (OBOProperty)
			session.getObjectFactory().createObject(negRegRelId, 
					OBOClass.OBO_PROPERTY, false);
			session.addObject(negRegRel);
			Link subProp = 
				new OBORestrictionImpl(negRegRel,OBOProperty.IS_A,
						regRel, false);
			negRegRel.addParent(subProp);
			negRegRel.setTransitiveOver(partOfRel);
		}

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
		if (item == null) {
			for (String label : TermUtil.getExactLabels(lo)) {
				if (label.equals(lo.getName())) {
					continue;
				}
				item = parseTerm(lo,label);
				if (item != null)
					break;
			}
		}
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
		if (lo instanceof OBOClass && 
				ReasonerUtil.getGenus((OBOClass)lo) != null) {
			report("PREDEFINED",lo,name,"already have a logical def");
			return null;
		}
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
			else if (target.equals(lo)) {
				report("SELF_REFERENCE",lo,name,"cannot regulate itself");
				logger.info("self reference: "+targetName);
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
				// remove old part_of
				if (replacePartOfs) {
					OBORestriction partOfLink;
					for (Link link : lo.getParents()) {
						if (link.getType().equals(partOfRel) &&
								link.getParent().equals(target)) {
							DeleteLinkHistoryItem delItem = new DeleteLinkHistoryItem(link);
							item.addItem(delItem);					
						}
					}
				}
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
		n = n.replace('_', ' '); // e.g. biological_process
		return name2obj.get(n);
	}

	public void apply(Collection<? extends HistoryItem> items) {
		DefaultOperationModel model = new DefaultOperationModel();
		model.setSession(session);
		for (HistoryItem item : items) {
			model.apply(item);
		}
	}



	protected Namer namer;
	public Namer getNamer() {
		return namer;
	}
	public void setNamer(Namer namer) {
		this.namer = namer;
	}

	public void useDefaultNamer() {
		namer = new RegulationTermNamer();
	}



}
