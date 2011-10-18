<%@page import="com.google.gson.Gson"%>
<%@page import="org.geneontology.gold.rules.json.ErrorForJson"%>
<%@page import="org.geneontology.gold.rules.AnnotationRuleViolation"%>
<%@page import="java.util.Set"%>
<%@page import="org.geneontology.web.services.Task"%>
<%@page import="java.io.PrintWriter"%>
<%@page import="java.util.List"%>
<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
    pageEncoding="ISO-8859-1" trimDirectiveWhitespaces="true"%>

	<%
	
		Task task = (Task)request.getAttribute("task");
		Set<AnnotationRuleViolation> annotationRuleViolations = (Set<AnnotationRuleViolation>)request.getAttribute("violations");

		request.setAttribute("removeAnnotations", new Boolean(true));
		
		List<Throwable> ex = null;
		
		if(task != null){
			
			ex = task.getException();
		
			if(ex != null && !ex.isEmpty()){
				PrintWriter pw = new PrintWriter(out);
				for(Throwable t: ex)
					t.printStackTrace(pw);
			
				pw.flush();
			}else if(annotationRuleViolations !=null && !annotationRuleViolations.isEmpty()){
				%>
				<jsp:include page="/servicesui/print_annotations_voilations.jsp"/>
				<%
			}else if(task == null || !task.isRunning()){
				ErrorForJson err = new ErrorForJson("task_completed");
				Gson gson = new Gson();
				out.println(gson.toJson(err));
			}
			
		}
		/*Gson gson = new Gson();
		
		if(annotationRuleViolations != null && annotationRuleViolations.size()>0){			
		
				int i =0;
				Vector<AnnotationRuleViolation> toRmove = new Vector<AnnotationRuleViolation>();
				List<AnnotationVoilationForJson> list = new ArrayList<AnnotationVoilationForJson>();
				for(AnnotationRuleViolation v: annotationRuleViolations){
					if(i>=2000){
						i=0;
						break;
					}
					toRmove.add(v);
					String msg = v.getMessage();
					String s = v.getSourceAnnotation() + "";
					String ruleId = v.getRuleId();
					GeneAnnotation ga = v.getSourceAnnotation();
					int lineNr = -1;
					String fileName = "";
					if(ga != null){
						lineNr =  ga.getSource() != null ? ga.getSource().getLineNumber() : -1;
						fileName = ga.getGafDocument();
					}
                    
                    AnnotationVoilationForJson av = new AnnotationVoilationForJson(msg, s, ruleId,lineNr, fileName);
                    list.add(av);
                    i++;
				}

				
				for(AnnotationRuleViolation v: toRmove){
					annotationRuleViolations.remove(v);
				}
				
				String jsonString= gson.toJson(list);
				
				out.println(jsonString);
				
			
				
			//annotationRuleViolations.clear();
			
		}else if(task == null || (task != null && !task.isRunning())){
			ErrorForJson error = new ErrorForJson("NO_DATA");
			out.println("["+gson.toJson(error)+"]");
		}*/
	
%>


