<#include "PageMacros.ftl"> 
<#include "NodeDetailsMacro.ftl"> 
<html>
    <head>
		<@stylize/>
    	<@genTitle pageName="Node Similarities"/>
    </head>	
	<body>
		<@header/>	
		<table class="threecol">
			<tr>
				<td id="left_bar">
					<@searchForm/>
					<hr class="divider"/>
					<@nodeMetaData/>
				</td>
				<td id="vertical_divider">
				</td>
				<td id="content_container">
					<#if node?exists>
						<h2>Similar to ${node.getId()}<#if node.getLabel()?has_content>: ${node.getLabel()}</#if></h2>
						<br/>
						<#if results?exists>
							<table class="scoredNodeResults">
							 	<tr><th style="width:190px;background-color:#fafafa;">Score</th><td style="background-color:#fafafa;">Node</td></tr>
								<#list results as hit>
									<tr>
										<th>
											${hit.score}
										</th>
										<td>
											${hit.node}
										</td>
									</tr>
								</#list>
							</table>
						<#else>
							No Results
						</#if>
					<#else>
						Node ${nodeId} not found. 
					</#if>
				</td>
			</tr>
		</table>
		<@footer/>
	</body>
</html>
