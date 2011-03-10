<%@page import="org.obolibrary.obo2owl.Owl2Obo"%>
<%@page import="owltools.graph.OWLGraphWrapper"%>
<%@page import="java.util.List"%>
<%@page import="java.util.Enumeration"%>
<%@page import="owltools.graph.OWLGraphEdge"%>
<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
	pageEncoding="ISO-8859-1"%>
<%@page import="org.semanticweb.owlapi.model.OWLAxiom"%>
<%@page import="org.semanticweb.owlapi.model.OWLSubClassOfAxiom"%>
<%@page import="org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom"%>
<%@page import="java.util.Set"%>
<%@page import="org.semanticweb.owlapi.model.OWLClassExpression"%>
<%@page import="java.util.Iterator"%>
<%@page import="org.semanticweb.owlapi.model.OWLObject"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">

<head>
	<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
	<title>Reasoning Service</title>
</head>

<body>

	<h1>Inferences Computation Results</h1>
	

	<%
		boolean  isTaskRunning = (Boolean)request.getAttribute("isTaskRunning");
	
		if(isTaskRunning){
	%>

		<script type='text/javascript'>
			setTimeout("location.reload(true)", 9000);
		</script>
	
		<center><img src="/images/progress-indicator.gif" alt="Request is in Progress" /></center>
		<p align="center">Your Request is in Progress</p>
	
	<%
		}
	%>


	<table>
		<tr>
		<td>Axiom Type</td><td>Class 1</td><td>Class 2</td>
		</tr>
		<%
		
			OWLGraphWrapper graph = (OWLGraphWrapper)request.getAttribute("graph");
			for(OWLAxiom axiom: (List<OWLAxiom>)request.getAttribute("axioms")){
				
				String label1 = null;
				String label2 = null;
				String axiomType = null;
				
				if(axiom instanceof OWLSubClassOfAxiom){
					OWLSubClassOfAxiom ax = (OWLSubClassOfAxiom)axiom;
					label1 = Owl2Obo.getIdentifier(ax.getSubClass()) +"[" + graph.getLabel(ax.getSubClass()) +"]";
					label2 = Owl2Obo.getIdentifier(ax.getSuperClass()) +"[" + graph.getLabel(ax.getSuperClass()) +"]";
					axiomType = "SUBCLASS_OF";
				}else if(axiom instanceof OWLEquivalentClassesAxiom){
					OWLEquivalentClassesAxiom ax = (OWLEquivalentClassesAxiom) axiom;
					Iterator<OWLClassExpression> classes = ax.getClassExpressions().iterator();
					OWLObject obj1 = classes.next();
					OWLObject obj2 = classes.next();
					label1 = Owl2Obo.getIdentifier(obj1) +"[" + graph.getLabel(obj1) +"]";
					label2 = Owl2Obo.getIdentifier(obj2) +"[" + graph.getLabel(obj2) +"]";
					axiomType = "EQUIVALENT";
					
				}else{
					continue;
				}
		%>
				
				<tr>
			 	<td><%= axiomType %></td><td><%= label1 %></td><td><%= label2 %></td>
				</tr>
		<%		
				
			}
			
		%>
		
		
	</table>

	<%
	String taskCompletionTime = (String)request.getAttribute("taskCompletionTime");
	%>
	<hr />
	<h4>The inference computation is completed in <%= taskCompletionTime %> seconds</h4>	

	
</body>


</html>