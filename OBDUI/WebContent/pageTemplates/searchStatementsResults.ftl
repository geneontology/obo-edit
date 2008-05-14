<#include "PageMacros.ftl">
<#include "NodeDetailsMacro.ftl"> 
<html>
	<head>
		<@genTitle pageName="Search Results"/>
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
					<#if searchTerm?exists>
						<h2><span style="font-weight:normal;">Search results for</span> ${searchTerm} </h2><br/>
					</#if>
					<#if resultStatements?exists>
						<table>
						<#list resultStatements as statement>
							<tr><td style="font-size:10px;background-color:#fafafa;">&bull;</td><td style="background-color:#fafafa;font-size:12px;">
							<@labelhref target=statement.sourceHref label=statement.sourceLabel/> 
					 		<@labelhref target=statement.relationHref label=statement.relationLabel/> 
					 		<@labelhref target=statement.targetHref label=statement.targetLabel/><br/>
					 		</td></tr>
						</#list>
						</table>
					<#else>
						No Results
					</#if>
				</td>
			</tr>
		</table>
		<@footer/>
	</body>
</html>
