<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
    pageEncoding="ISO-8859-1"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
<title>Login Page</title>
<script src="https://browserid.org/include.js" type="text/javascript"></script>
</head>
<body>
<h1>Please <a href="javascript:showLoginWindow();">Login</a>....through <a href="https://browserid.org/">browserid.org</a> website by providing your email address.</h1>
<script type="text/javascript">

function showLoginWindow(){
	
	navigator.id.getVerifiedEmail(function(assertion) {
	    if (assertion) {
	    	submitForm(assertion);
	    } else {
			alert("Login is failed. Please try again.");
	    }
	});	
	
}


function submitForm(assertion){
	var frm_element = document.getElementById('assertionValue');
	frm_element.value=assertion;
	
	document.forms[0].submit();
	
}

</script>

			<form id="reloadform" action="/gold">
				<%
				for(Object param: request.getParameterMap().keySet()){
					%>
					<input type="hidden" name="<%= param %>" value="<%= request.getParameter(param.toString()) %>" />
					<%
				}
				
				%>
				<input type="hidden" id="assertionValue" name="assertion" value="" />	
							
			</form>


</body>
</html>