<%@page import="org.geneontology.gold.rules.json.AnnotationVoilationForJson"%>
<%@page import="org.geneontology.gold.rules.json.ErrorForJson"%>
<%@page import="com.google.gson.Gson"%>
<%@page import="java.util.Comparator"%>
<%@page import="java.util.ArrayList"%>
<%@page import="java.util.List"%>
<%@page import="java.util.Collections"%>
<%@page import="java.io.PrintWriter"%>
<%@page import="org.geneontology.gold.rules.AnnotationRuleViolation"%>
<%@page import="java.util.Set"%>
<%@page import="org.geneontology.web.services.Task"%>
<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
    pageEncoding="ISO-8859-1" trimDirectiveWhitespaces="true"%>

	<%
		Task task = (Task)request.getAttribute("task");
		Set<AnnotationRuleViolation> annotationRuleViolations = (Set<AnnotationRuleViolation>)request.getAttribute("violations");
		String sessionId = request.getParameter("id");
		List<Throwable> ex = null;
		Gson gson = new Gson();
		//HttpSession session = request.getSession(true);
	///	out.println("{ \"QcChecksOutput\": {");
		
		if(annotationRuleViolations == null){
			ErrorForJson error =  new ErrorForJson("Annotaiton Voilations can not be null");
			out.println(gson.toJson(error));
			//out.println("\t{\"error\": \"The server internal error: Please contect to the system administrator.\"}");
		}else if(sessionId != null && task != null){		
		
			if(annotationRuleViolations != null){
			//	PrintWriter pw = new PrintWriter(out);
		///		out.println("\t\"voilations\": {");
		
			//	while(task.isRunning()){
					/*	try{
							Thread.sleep(120000);
						}catch(Exception e){
							
							out.println("\t{\"error\": \""+ e.getMessage() +"\"},");
		
							//ex.printStackTrace(pw);
						}*/
					
					
					
						ex = task.getException();
					
						if(ex != null){
							for(Throwable t: ex){
								ErrorForJson error =  new ErrorForJson(t.getMessage());
								out.println(gson.toJson(error));
							}
							
								//	out.println("\t{\"error\": \""+ t.getMessage() +"\"},");
							//	ex.printStackTrace(pw);
							//pw.flush();
						}else{
						//if(annotationRuleViolations != null){
							int i=0;
							int Annsize = annotationRuleViolations.size();
							List<AnnotationVoilationForJson> list = new ArrayList<AnnotationVoilationForJson>();
							
							if(Annsize>0){
								out.println("[");
							}
							
							for(AnnotationRuleViolation v: annotationRuleViolations){
								String msg = v.getMessage();
								msg = msg.replaceAll("\t", "\\\\t");
								String s = v.getSourceAnnotation() + "";
								s = s.replaceAll("\t", "\\\\t");
								
								int lineNumber = -1;
								if(v.getSourceAnnotation().getSource()!=null){
									lineNumber = v.getSourceAnnotation().getSource().getLineNumber();
								}
								
								/*if(i==0 && session.getAttribute("hasOutputannotations") != null){
									out.println(",");
								}*/
								
								AnnotationVoilationForJson av = new AnnotationVoilationForJson(msg, s, v.getRuleId() == null ? "": v.getRuleId(), lineNumber, "");
							//	list.add(av);
								out.println(gson.toJson(av));
								
								if(i<Annsize){
									out.println(",");
								}
								i++;
								//list.add(av);
							/*	out.println("\t\t\"voilation\": {");
								out.println("\t\t\t\"message\": \"" + msg + "\",");
								out.println("\t\t\t\"annotation\": \"" + s + "\"");
								out.println("\t\t\t\"rule-id\": \"" + v.getRuleId() + "\"");
								out.println("\t\t\t\"line-number\": \"" + v.getSourceAnnotation().getSource() != null ? v.getSourceAnnotation().getSource().getLineNumber() : "" + "\"");
								out.println("\t\t}");*/
								i++;
								
								/*if(i < Annsize){
									out.println("\t\t,");
								}*/
				
							}
							
							
						//	System.out.println("......................Clearing the annotations: " + annotationRuleViolations.size());
							annotationRuleViolations.clear();
						//	String jsonText= gson.toJson(list);							
						//	out.println(jsonText);
						}
						
						
					//	}
					
					
				//	}
					
				///out.println("\t}");
				
				
				
				
				
				
			}
		}else  if(task == null || (task != null && !task.isRunning()))
		   out.println("NO_DATA");
		else if(sessionId == null){
			sessionId = session.getId();
			
			out.println("session:"+sessionId);
		}
	   
	
%>


