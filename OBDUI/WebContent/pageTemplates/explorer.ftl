<#include "PageMacros.ftl"> 
<html>
	<head>
    	<@genTitle pageName="Node Search"/>
    	<@stylize/>
	</head>
	<body>
        
   		<@header/>
	
		<table class="threecol">
			<tr>
				<td id="left_bar">
					<@searchForm/>
				</td>
				<td id="vertical_divider">
				</td>
				<td id="content_container">
					<@dataSourceList/>
	     			<@mappedPathsList/>
				</td>
			</tr>
		</table>
		
		<@footer/>
		
	</body>
</html>
