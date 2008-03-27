<#include "PageMacros.ftl"> 
<html>
	<head>
		<title>
			OBD Search
		</title>
		<link rel="stylesheet" type="text/css" href="/OBDUI/css/obd-main.css"/>
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
