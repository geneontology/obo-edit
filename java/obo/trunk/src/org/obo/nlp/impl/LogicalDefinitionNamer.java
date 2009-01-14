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

/**
 * @author cjm
 *
 */
public class LogicalDefinitionNamer extends AbstractNamer implements Namer {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(LogicalDefinitionNamer.class);

	public Collection<String> constructNames(LinkedObject lo) {
		Collection<String> names = new LinkedList<String>();
		if (TermUtil.isIntersection(lo)) {
			StringBuilder name = new StringBuilder();
			for (Link dl : lo.getParents()) {
				if (!TermUtil.isIntersection(dl))
					continue;
				if (name.length() > 0)
					name.append(" -and-");
				if (!dl.getType().equals(OBOProperty.IS_A)) {
					String relName = dl.getType().getName();
					if (relName == null)
						relName = dl.getType().getID();
					name.append(" "+relName);
				}
				
				name.append(" "+getName(dl.getParent())); // TODO - recursive
			}

			names.add(name.toString());

		}
		else if (TermUtil.isUnion(lo)) {
			StringBuilder name = new StringBuilder();
			for (Link link : lo.getParents()) {
				if (TermUtil.isUnion(link)) {
					if (name.length() > 0)
						name.append(" -or- ");
					name.append(getName(link.getParent()));
				}
			}
			names.add(name.toString());
		}
		else {
			names.add(lo.getID());
		}
		return names;
	}
	
	protected String getName(LinkedObject lo) {
		if (lo == null) {
			return "?";
		}
		String name = lo.getName();
		
		if (name == null) {
			for (String sn : constructNames(lo)) {
				name = sn;
				break;
			}
		}
		if (name == null) {
			name = lo.getID();
		}
		return name;
	}

	public Collection<String> constructTextDefs(LinkedObject lo) {
		// TODO Auto-generated method stub
		return null;
	}



}
