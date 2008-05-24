package org.obd.ws.coreResource.sorter;

import java.util.Comparator;
import freemarker.template.SimpleHash;
import freemarker.template.TemplateModelException;

public class StatementHashSubjectComparator implements Comparator<SimpleHash> {

	public int compare(SimpleHash o1, SimpleHash o2) {

		
		try {
			String subject1 = ((SimpleHash)o1).get("sourceLabel").toString();
			String subject2 = ((SimpleHash)o2).get("sourceLabel").toString();
			return subject1.compareToIgnoreCase(subject2);
		} catch (TemplateModelException e) {
			e.printStackTrace();
		}
		return 0;
		
	}
	
}