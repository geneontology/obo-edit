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
					<#if resultCounts?exists>
						<#list resultCounts as result>
							${result.count}
							<#if (result.count>1) >
								results
							<#else>
								result
							</#if> in ${result.category}:	
						<table class="std_table">
							<#list resultNodes as node>
								<#if node.category == result.category>
									<tr>
										<td style="width:400px;"><@labelhref target=node.nodeHref label=node.nodeLabel/></td><td style="width:150px;">${node.nodeId}</td><td style="width:200px;">${node.source}</td>
									</tr>
								</#if>
							</#list>
						</table>
						<br/>	
					</#list>	
					<#else>
						No Results.
					</#if>
				</td>
			</tr>
		</table>
		<@footer/>
	</body>
</html>
