<%@page import="java.util.ArrayList"%>
<%@page import="org.geneontology.gold.rules.json.PredictionForJson"%>
<%@page import="owltools.gaf.inference.Prediction"%>
<%@page import="java.util.Hashtable"%>
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
		Hashtable<String, Set<Prediction>> inferredAnnotations= (Hashtable<String, Set<Prediction>>)request.getAttribute("inferredAnnotations");

		List<Throwable> ex = null;

		//when the variable is true then print predictions
		boolean printPredictions = false;
		
		
		if(inferredAnnotations !=null){	
			for(Set<Prediction> predictions: inferredAnnotations.values()){
				if(!predictions.isEmpty()){
					printPredictions = true;
					break;
				}
			}
		}
		
		if(task != null){
			
			ex = task.getException();
		
			if(ex != null && !ex.isEmpty()){
				PrintWriter pw = new PrintWriter(out);
				for(Throwable t: ex)
					t.printStackTrace(pw);
			
				pw.flush();
			}else if(printPredictions){

				List<PredictionForJson> list = new ArrayList<PredictionForJson>();
				Gson gson = new Gson();
				
				
				for(Set<Prediction> predictions: inferredAnnotations.values()){
					for(Prediction v: predictions){
						String fileName = v.getGeneAnnotation().getGafDocument();
						PredictionForJson pj = new PredictionForJson(v.getGeneAnnotation().toString(), v.isRedundantWithExistingAnnotations(), v.isRedundantWithOtherPredictions(), fileName);
						list.add(pj);
					}
					
					predictions.clear();
				}
				
								
				
				out.println(gson.toJson(list));
				
				
			
			}else if(task == null || !task.isRunning()){
				ErrorForJson err = new ErrorForJson("task_completed");
				Gson gson = new Gson();
				out.println(gson.toJson(err));
			}
			
		}
	
%>


