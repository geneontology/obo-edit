<#include "/PageMacros.ftl"> 
<html>
	<head>
    	<@genTitle pageName="OBD Use Cases"/>
    	<@stylize/>
	</head>
	<body>
        
   		<@header/>
	
		<table class="threecol">
			<tr>
				<td id="left_bar">
					<@searchForm/>
					<hr class="divider"/>
					<a style="font-size:12px;" href="/${contextName}/usecases/">OBD Use Cases&nbsp;&raquo;</a>
				</td>
				<td id="vertical_divider">
				</td>
				<td id="content_container">
					<#include includePage> 
				</td>
			</tr>
		</table>
		
		<@footer/>
		
	</body>
</html>
