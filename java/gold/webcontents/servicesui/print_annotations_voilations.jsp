 <%@page import="owltools.gaf.GeneAnnotation"%>
<%@page import="com.google.gson.Gson"%>
<%@page import="java.util.ArrayList"%>
<%@page import="org.geneontology.gold.rules.json.AnnotationVoilationForJson"%>
<%@page import="org.geneontology.gold.rules.AnnotationRuleViolation"%>
<%@page import="java.util.List"%>
<%@page import="java.util.Set"%>
<%
	Set<AnnotationRuleViolation> annotationRuleViolations = (Set<AnnotationRuleViolation>)request.getAttribute("violations");
	List<AnnotationVoilationForJson> list = new ArrayList<AnnotationVoilationForJson>();
	Gson gson = new Gson();
	for(AnnotationRuleViolation v: annotationRuleViolations){
		String msg = v.getMessage();
		String source = v.getAnnotationRow() + "";
		String ruleId = v.getRuleId();
		GeneAnnotation ga = v.getSourceAnnotation();
		int lineNr = v.getLineNumber();
		String fileName = v.getGafDoument();
		AnnotationVoilationForJson av = new AnnotationVoilationForJson(msg,source, ruleId, lineNr, fileName);
		list.add(av);
	}
	
	Boolean removeAnnotations = (Boolean)request.getAttribute("removeAnnotations");
	
	if(removeAnnotations != null && removeAnnotations.booleanValue())
		annotationRuleViolations.clear();
	
	out.println(gson.toJson(list));
%>