package org.obd.ws.coreResource.sorter;

import java.util.Comparator;

import freemarker.template.SimpleHash;
import freemarker.template.TemplateModelException;

public class StatementComparator implements Comparator<SimpleHash> {

	public int compare(SimpleHash o1, SimpleHash o2) {
		
		try {
			String relation1 = ((SimpleHash)((SimpleHash)o1).get("statement")).get("relationLabel").toString();
			String relation2 = ((SimpleHash)((SimpleHash)o2).get("statement")).get("relationLabel").toString();
			
			//System.out.println("Comparing " + relation1 + " to " + relation2 + ":\t" + relation1.compareToIgnoreCase(relation2));
			
			if (relation1.compareToIgnoreCase(relation2) < 0) {
				return -1;
			} else if (relation1.compareToIgnoreCase(relation2) == 0) {
				
				
				String subject1 = ((SimpleHash)o1.get("statement")).get("sourceLabel").toString();
				String subject2 = ((SimpleHash)o2.get("statement")).get("sourceLabel").toString();
				
				return subject1.compareTo(subject2);
				
				
			} else if (relation1.compareToIgnoreCase(relation2) > 0) {
				return 1;
			}
			
		} catch (TemplateModelException e) {
			e.printStackTrace();
			
		}
		
		System.exit(-1);
		return 0;
	}
	
	
}