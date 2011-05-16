package org.geneontology.gold.rules;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.log4j.Logger;
import org.geneontology.gaf.hibernate.GeneAnnotation;
import org.jdom.Element;

/**
 * This class implements execution of the regular expression rules. All the regular expression rules
 * are run by the single instance of this class.
 * @author Shahid Manzoor
 *
 */
public class AnnotationRuglarExpressionFromXMLRule extends
		AbstractAnnotationRule {

	/**
	 * 
	 * @param The parameter is the list of the {@link Element} objects. These elements objects are 
	 * build from the annotation-qc.xml xml file. The each element object points to 'script' xml tag.
	 */
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

	/**
	 * 
	 * @param This method iterates through the script xml elements to get the regular expression. 
	 * It gets the 'title' element (child of the 'rule' element). During the execution of the rule title
	 * is used as an error message when the regular expression is matched.
	 */
	private  void init(List regexList){
		rules = new ArrayList<AnnotationRuglarExpressionFromXMLRule.RuleData>();
		
		try {
		    Iterator itr = regexList.iterator();
		    
		    while(itr.hasNext()){
		    	Element script = (Element) itr.next();
		    	
		    	String regex = script.getTextNormalize();
		    	boolean isCaseInsensitive = regex.endsWith("/i");
		    	
		    	//java doesn't like the /^ switch so it is replaced by ^
		    	regex = regex.replace("/^", "^");
		    	//java does not support the /i case in-sensitivity switch so it is removed
		    	regex = regex.replace("/i", "");
		    	
		    	//title is child of the rule element
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
