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
					<hr class="divider"/>
					<@geneMetaData/>
				</td>
				<td id="vertical_divider">
				</td>
				<td id="content_container">
					<h2>Gene ${geneId}<#if geneLabel?exists>: ${geneLabel}</#if></h2><br/>
					
					<script type="text/javascript" src="/OBDUI/js/table-toggle.js"></script>
					<a href="#" onclick="toggleTable('genotypes','${hostname}');return false;">
						<div class="nodeDetailBox"><img id="genotypes_image" src="/OBDUI/images/plus-box.gif" class="expandoImage"/>
							Genotypes				
						<div>
					</a>
					<table class="statementTableWrapper">
						<tr>
							<td id="genotypes" style="display:none;">
								<#list genotypes as genotype>
									${genotype}<br/>
								</#list>
							</td>
						</tr>
					</table>
				</td>
			</tr>
		</table>
		<@footer/>
	</body>
</html>

<#macro geneMetaData>
	<table class="mdInfo">
		<tr>
			<th>Organism:</th>
			<td>Get
		</tr>
		<tr>
			<th>DB Xrefs:</th>
			<td>List</td>
		</tr>
	</table>
</#macro>
