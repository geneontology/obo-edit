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
						<h2>Similar to ${node.id}: ${node.label}</h2>
						<br/>
						<#if results?exists>
							<table class="scoredNodeResults">
							 	<tr><th style="width:190px;background-color:#fafafa;">Node</th><td style="background-color:#fafafa;">Score</td></tr>
								<#list results as scoredNodeHit>
									<tr>
										<td>
											<@nodeHashHref nodeHash=scoredNodeHit/>
										</td>
										<th>
											<a href="${scoredNodeHit.spHref}"/>${scoredNodeHit.score}</a>
										</th>
										
									</tr>
								</#list>
							</table>
						<#else>
							No Results
						</#if>
					<#else>
						Node ${node.id} not found. 
					</#if>
				</td>
			</tr>
		</table>
		<@footer/>
	</body>
</html>
