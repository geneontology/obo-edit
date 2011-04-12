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

	
	public AnnotationRuglarExpressionFromXMLRule(){
		init();
	}

	private static Logger LOG = Logger.getLogger(AnnotationRuglarExpressionFromXMLRule.class);
	
	private static String rules_xml_file_location = "http://www.geneontology.org/quality_control/annotation_checks/annotation_qc.xml";
	
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

	private  void init(){
		SAXBuilder builder = new SAXBuilder();
		rules = new ArrayList<AnnotationRuglarExpressionFromXMLRule.RuleData>();
		
		try {
			Document doc = builder.build(new URL(rules_xml_file_location));
		    XPath regexRule = XPath.newInstance("//implementation/script[@language='regex']");			
			
		    Iterator itr = regexRule.selectNodes(doc).iterator();
		    
		    while(itr.hasNext()){
		    	Element script = (Element) itr.next();
		    	
		    	String regex = script.getTextNormalize();
		    	
		    	Element title= script.getParentElement().getParentElement().getChild("title");
		    	String titleString = "";
		    	if(title != null){
		    		titleString = title.getTextNormalize();
		    	}
		    	
		    	RuleData data = new RuleData(Pattern.compile(regex), titleString);
		    	
		    	rules.add(data);
		    }
		    
		    
		    
			/*Iterator itr = doc.getRootElement().getChildren("rule").iterator();
			
			while(itr.hasNext()){
				Element rule = (Element)itr.next();
				Element impList = rule.getChild("implementation_list");
				if(impList == null)
					return;
				
				Iterator impItr= impList.getChildren().iterator();
				
				while(impItr.hasNext()){
					Element imp = (Element)impItr.next();
					
					Element script = imp.getChild("script");
					
					if(script == null)
						return;
					
					String langAttr = script.getAttributeValue("language");
					if(!"regex".equals(langAttr))
						return;
					
					String regex = script.getTextNormalize();
					
					rules.put(regex, rule);
				}
		    
		    
				
			}*/
			
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
