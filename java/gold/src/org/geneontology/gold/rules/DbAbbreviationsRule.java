package org.geneontology.gold.rules;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.HashSet;
import java.util.Set;

import org.apache.log4j.Logger;
import org.geneontology.conf.GoConfigManager;
import org.geneontology.gaf.hibernate.GeneAnnotation;

public class DbAbbreviationsRule extends AbstractAnnotationRule {

	private static final Logger LOG = Logger.getLogger(DbAbbreviationsRule.class);
	
	private static final HashSet<String> abbreviations = buildAbbreviations();
	
	private static HashSet<String> buildAbbreviations(){
		HashSet<String> set = new HashSet<String>();
		
		try{
			
			BufferedReader reader = new BufferedReader(new FileReader(new File(GoConfigManager.getInstance().getGoXrfAbbsLocation())));
			
			String line = null;
			while ((line =reader.readLine()) != null) {
	
				if(line.startsWith("!"))
					continue;
					
				String data[] = line.split(":");
				
				if(data.length==2 && "abbreviation".equals(data[0]) ){
					set.add(data[1].trim());
				}
				
			}			
			
		}catch(Exception ex){
			LOG.error("Cann't read Go.xrf_abbs file at the location " + GoConfigManager.getInstance().getGoXrfAbbsLocation(),
					ex);
		}
		
		return set;
		
	}
	
	@Override
	public Set<AnnotationRuleViolation> getRuleViolations(GeneAnnotation a) {
		
		HashSet<AnnotationRuleViolation> set = new HashSet<AnnotationRuleViolation>();
		
	 	if (!abbreviations.contains( a.getBioentityObject().getDb() )){
	 		AnnotationRuleViolation v =  new AnnotationRuleViolation("DB reference is not found in the Go.xref_abbs file", a);
	 		v.setRuleId(getRuleId());
	 		set.add(v);
	 	}
		
	 	String id = a.getBioentityObject().getId();
	 	
	 	
	 	
		return set;
	}	
	
}
