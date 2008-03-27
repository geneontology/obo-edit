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
					<hr class="divider"/>
					<@otherFormats/>
				</td>
				<td id="vertical_divider">
				</td>
				<td id="content_container">

					<#list ["Disease","Gene","Anatomical Part","Other"] as searchCategory>
						${searchCategory} information:
						
						<table class="std_table">
							<#list nodeProperties as node>
							<#if searchCategory == node.category>
							<tr>
								<th style="width:400px;">${node.linkedLabel}</th><td style="width:120px;">${node.id}</td><td style="width:120px;">${node.source}</td>
							</tr>
							</#if>
							</#list>
						</table>
						<br/>	
					</#list>
					
					<#--
					<table class="std_table">
					<#list nodeProperties as node>
						<tr>
							<td>${node.label}</td><td>${node.linked}</td><td>${node.source}</td>
						</tr>
					</#list>
					</table>
					-->
				</td>
			</tr>
		</table>
		
		<@footer/>
		
	</body>
</html>
