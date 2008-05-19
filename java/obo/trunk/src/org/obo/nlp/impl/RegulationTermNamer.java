package org.obo.nlp.impl;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.nlp.Namer;
import org.obo.util.ReasonerUtil;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class RegulationTermNamer extends AbstractNamer implements Namer {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(RegulationTermNamer.class);

	public Collection<String> constructNames(LinkedObject lo) {
		Collection<String> names = new LinkedList<String>();
		if (TermUtil.isIntersection(lo)) {
			OBOClass genus = ReasonerUtil.getGenus((OBOClass)lo);
			if (isSubclassByName(genus, "biological regulation")) {
				Collection<String> currentLabels = TermUtil.getLabels(lo);
				Collection<Link> diffs = ReasonerUtil.getDifferentia((OBOClass)lo);
				if (diffs.size() == 1) {
					Link diff = diffs.iterator().next();
					Collection<String> targetLabels = TermUtil.getExactLabels(diff.getParent());
					Collection<String> bases = new HashSet<String>();
					if (diff.getType().getID().contains("negatively_regulates")) {
						bases.add("negative regulation of");
						bases.add("down-regulation of");
					}
					else if (diff.getType().getID().contains("positively_regulates")) {
						bases.add("positive regulation of");
						bases.add("up-regulation of");
					}
					else {
						bases.add("regulation of");
					}
					
					for (String base : bases) {
						for (String targetLabel : targetLabels) {
							String newName = base+" "+targetLabel;
							if (!currentLabels.contains(newName))
								names.add(newName);
						}
					}
				}
			}
		}
		return names;
	}

	public Collection<String> constructTextDefs(LinkedObject lo) {
		// TODO Auto-generated method stub
		return null;
	}
	
	

}
