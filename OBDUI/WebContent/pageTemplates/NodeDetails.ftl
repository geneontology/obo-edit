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
						<#--
						<@phat id=encodedId/>
						<hr class="divider"/>
						<@nodeMetaData/>
						-->
						<@altViews nodeViews=nodeViews/>
					</#if>
				</td>
				<td id="vertical_divider">
				</td>
				<td id="content_container">
					<#if node?exists>
						<h2>${node.id}
							<#if node.isComposed?exists>:<@labelDecompose node=node.composedNode/></h2>
							<#else>
								<#if node.label?has_content>: ${node.label}</#if></h2>
							</#if>
						<br/>
						<@nodeDetailsTable/>
					<#else>
						<h2>Node ${id} Not Found</h2>
					</#if>
				</td>
			</tr>
		</table>
		<@footer/>
	</body>
</html>
