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
				</td>
				<td id="vertical_divider">
				</td>
				<td id="content_container">
					<h2>
						<#list nodes as node>
							<@labelhref target=node.href label=node.id/>: ${node.label}<br/>
						</#list>
					</h2><br/>
					<@nodeDetailsTable/>
				</td>
			</tr>
		</table>
		<@footer/>
	</body>
</html>
