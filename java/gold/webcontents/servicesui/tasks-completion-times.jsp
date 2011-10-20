<%@page import="com.google.gson.Gson"%>
<%@page import="java.util.ArrayList"%>
<%@page import="org.geneontology.gold.rules.json.TaskStatusForJson"%>
<%@page import="java.util.List"%>
<%@page import="org.geneontology.web.services.Task"%>
<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
    pageEncoding="ISO-8859-1"%>

    
 	<%
		Task task = (Task)request.getAttribute("task");
 	
 		List<TaskStatusForJson> list = new ArrayList<TaskStatusForJson>();
 		if(task != null){
			String ontology = "";
			for (String opName : task.getCompletedOperations()) {
				String[] st = opName.split("--");
				String opLocalName = opName;
				if (st.length > 1) {
					opLocalName = st[0];
				}
	
				long stTime = task.getStartTime(opName);
				long endTime = task.getEndTime(opName);

				TaskStatusForJson taskData = new TaskStatusForJson(opLocalName, stTime, endTime);
				list.add(taskData);
				
			}			
 			
 		}	
 		
		Gson gson = new Gson();
		String jsonText= gson.toJson(list);
		out.println(jsonText);
		
	%>
 