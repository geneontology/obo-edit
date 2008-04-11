<#include "PageMacros.ftl"> 
<#include "NodeDetailsMacro.ftl"> 
<html>
    <@genTitle pageName="Node Details"/>	
	<body>
		<@header/>	
		<table class="threecol">
			<tr>
				<td id="left_bar">
					<@searchForm/>
					<#if node?exists>
						<hr class="divider"/>
						<@phat id=node.getId()/>
						<hr class="divider"/>
						<@nodeMetaData/>
					</#if>
				</td>
				<td id="vertical_divider">
				</td>
				<td id="content_container">
					<#if node?exists>
						<h2>${node.getId()}<#if node.getLabel()?has_content>: ${node.getLabel()}</#if></h2>
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
