package org.geneontology.gold.rules;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.log4j.Logger;
import org.geneontology.gaf.hibernate.GeneAnnotation;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;
import org.jdom.xpath.XPath;

public class AnnotationRuglarExpressionFromXMLRule extends
		AbstractAnnotatioRule {

	
	public AnnotationRuglarExpressionFromXMLRule(List regexList){
		init(regexList);
	}

	private static Logger LOG = Logger.getLogger(AnnotationRuglarExpressionFromXMLRule.class);
	
	
	private  List<RuleData> rules;
	
	public Set<AnnotationRuleViolation> getRuleViolations(GeneAnnotation a) {

		if(a == null){
			throw new IllegalArgumentException("GeneAnnotation argument is null");
		}
		
		HashSet<AnnotationRuleViolation> voilations = new HashSet<AnnotationRuleViolation>();

		for(RuleData data: rules){
			getRuleViolations(voilations, a, data);
		}
		
		return voilations;
	}
	
	private void getRuleViolations(Set<AnnotationRuleViolation> voilations, GeneAnnotation ann, RuleData data){

		Matcher m = data.pattern.matcher(ann.toString());
		
		if(m.find()){
			AnnotationRuleViolation v = new AnnotationRuleViolation(data.errorMsg, ann);
			voilations.add(v);
		}
		
		
	}

	private  void init(List regexList){
		rules = new ArrayList<AnnotationRuglarExpressionFromXMLRule.RuleData>();
		
		try {
		    Iterator itr = regexList.iterator();
		    
		    while(itr.hasNext()){
		    	Element script = (Element) itr.next();
		    	
		    	String regex = script.getTextNormalize();
		    	boolean isCaseInsensitive = regex.endsWith("/i");
		    	
		    	regex = regex.replace("/^", "^");
		    	regex = regex.replace("/i", "");
		    	
		    	Element title= script.getParentElement().getParentElement().getParentElement().getChild("title");
		    	String titleString = "";
		    	if(title != null){
		    		titleString = title.getTextNormalize();
		    	}
		    	
		    	Pattern pattern = null;
		    	
		    	if(isCaseInsensitive)
		    		pattern = Pattern.compile(regex, Pattern.CASE_INSENSITIVE);
		    	else
		    		pattern = Pattern.compile(regex);
		    	
		    	RuleData data = new RuleData(pattern, titleString);
		    	
		    	rules.add(data);
		    }
		    
		} catch (Exception e) {
			LOG.error(e.getMessage(), e);
		}
	}
	
	private class RuleData{
		
		public RuleData(Pattern pattern, String errorMsg){
			this.pattern = pattern;
			this.errorMsg = errorMsg;
		}
		
		private Pattern pattern;
		private String errorMsg;
	}
	
}
