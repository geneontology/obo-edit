<#include "PageMacros.ftl">
<#include "NodeDetailsMacro.ftl"> 
<html>
	<head>
		<@genTitle pageName="Query Results"/>
		<@stylize/>
	</head>
	<body>
        <@header/>
		<table class="threecol">
			<tr>
				<td id="left_bar">
					<@searchForm/>
					<hr class="divider"/>
				</td>
				<td id="vertical_divider">
				</td>
				<td id="content_container">
					<#if queryString?exists>
						<h2><span style="font-weight:normal;">Query: ${queryString}</span> </h2><br/>
						<br/>
						<#list resultNodes as node>
							<@nodeHashHref nodeHash=node/><br/> 
						</#list>
					</#if>
				</td>
			</tr>
		</table>
		<@footer/>
	</body>
</html>
