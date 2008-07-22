package org.obd.ws.coreResource.sorter;

import java.util.Comparator;

import freemarker.template.SimpleHash;
import freemarker.template.TemplateModelException;

public class NodeScoreComparator implements Comparator<SimpleHash>{

	public int compare(SimpleHash o1, SimpleHash o2) {
		try {
			Double score1 = Double.parseDouble(((SimpleHash)o1).get("contentScore").toString());
			Double score2 = Double.parseDouble(((SimpleHash)o2).get("contentScore").toString());
			return score1.compareTo(score2);
		} catch (TemplateModelException e) {
			e.printStackTrace();
		}
		return 0;
	}
	
	
}