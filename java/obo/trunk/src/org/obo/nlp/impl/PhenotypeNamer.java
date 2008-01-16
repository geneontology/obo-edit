package org.obo.nlp.impl;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.nlp.Namer;
import org.obo.util.ReasonerUtil;
import org.obo.util.TermUtil;

public class PhenotypeNamer extends AbstractNamer implements Namer {

	public Collection<String> constructNames(LinkedObject lo) {
		Collection<String> names = new LinkedList<String>();
		if (TermUtil.isIntersection(lo)) {
			OBOClass genus = ReasonerUtil.getGenus((OBOClass)lo);
			if (isSubclassByName(genus, "quality")) {
				StringBuffer name = new StringBuffer();
				name.append(genus.getName());
				boolean isRelational = false;
				for (OBOClass c : ReasonerUtil.getDifferentiaByType((OBOClass)lo, "towards")) {
					name.append(c.getName());
					isRelational = true;
				}
				for (OBOClass c : ReasonerUtil.getDifferentiaByType((OBOClass)lo, "inheres_in")) {
					if (isRelational) {
						name.insert(0,c.getName()+" ");
					}
					else {
						name.append(" "+c.getName());
					}
				}
				names.add(name.toString());
			}
		}
		return names;
	}

	public Collection<String> constructTextDefs(LinkedObject lo) {
		// TODO Auto-generated method stub
		return null;
	}
	

}
