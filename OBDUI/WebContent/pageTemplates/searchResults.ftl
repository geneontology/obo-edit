<#include "PageMacros.ftl"> 
<html>

	<body>
        
        <@genTitle pageName="Search Results"/>
        
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
				</td>
			</tr>
		</table>
		<@footer/>
	</body>
</html>
