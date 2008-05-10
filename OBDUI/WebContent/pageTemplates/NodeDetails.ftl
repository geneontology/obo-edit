<#include "PageMacros.ftl"> 
<#include "NodeDetailsMacro.ftl"> 
<html>
	<head>
		<@stylize/>
    	<@genTitle pageName="Node Details"/>
    </head>	
	<body>
		<@header/>	
		<table class="threecol">
			<tr>
				<td id="left_bar">
					<@searchForm/>
					<#if node?exists>
						<hr class="divider"/>
						<@phat id=encodedId/>
						<hr class="divider"/>
						<@nodeMetaData/>
					</#if>
				</td>
				<td id="vertical_divider">
				</td>
				<td id="content_container">
					<#if node?exists>
						<h2>${nodeId}<#if nodeLabel?has_content>: ${nodeLabel}</#if></h2>
						<#if nodeDefinition?exists>
							<span class="nodeDef">${nodeDefinition}</span>
							<br/>
						</#if>
						<br/>
						<@nodeDetailsTable/>
					<#else>
						<h2>Node ${nodeId} Not Found</h2>
					</#if>
				</td>
			</tr>
		</table>
		<@footer/>
	</body>
</html>
