package org.obo.nlp.impl;

import java.util.Collection;
import java.util.LinkedList;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOClass;
import org.obo.nlp.Namer;
import org.obo.util.ReasonerUtil;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class SimpleLossyNamer extends AbstractNamer implements Namer {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SimpleLossyNamer.class);

	public SimpleLossyNamer() {
		// TODO Auto-generated constructor stub
	}

	public Collection<String> constructNames(LinkedObject lo) {
		Collection<String> names = new LinkedList<String>();
		if (TermUtil.isIntersection(lo)) {
			StringBuffer name = new StringBuffer();
			// Example:
			// spermatocyte nucleus
			// cysteine biosynthesis
			for (Link dl : ReasonerUtil.getDifferentia((OBOClass)lo)) {
				name.append(dl.getParent().getName());
			}
			name.append(ReasonerUtil.getGenus((OBOClass)lo).getName());
			names.add(name.toString());
		}
		return names;
	}
	
	public Collection<String> constructTextDefs(LinkedObject lo) {
		return new DefaultNamer().constructTextDefs(lo);
	}

}
