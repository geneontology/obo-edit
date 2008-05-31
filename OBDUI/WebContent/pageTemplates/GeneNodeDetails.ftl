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
					Node Metadata goes here.
				</td>
				<td id="vertical_divider">
				</td>
				<td id="content_container">
					<h2>Gene ${geneId}<#if geneLabel?exists>: ${geneLabel}</#if></h2><br/>
					
					<script type="text/javascript" src="/OBDUI/js/table-toggle.js"></script>
					<#if genotypes?exists>
						<a href="#" onclick="toggleTable('genotypes','${hostname}');return false;">
							<div class="nodeDetailBox"><img id="genotypes_image" src="/OBDUI/images/plus-box.gif" class="expandoImage"/>
								Genotype Comparisons				
							</div>
						</a>
						<table class="statementTableWrapper">
							<tr>
								<td id="genotypes" style="display:none;">
									<span style="font-weight:normal;padding:4px;font-size:12px;background-color:#FFEEEE;">Basic Similarity Score</span>  <span style="font-weight:normal;padding:4px;font-size:12px;background-color:#EEFFEE;">Information Content Ratio</span><br/><br/>
									<table style="background-color:#aaaaaa;border-spacing:1px;">
										<tr>
											<td style="font-size:11px;padding:1px;width:150px;text-align:center;font-weight:bold;">Genotype #</td>
											<#list 1..genotypeComparaScores?size as num>
												<td style="font-size:11px;font-weight:bold;padding:1px;width:40px;text-align:center;">${num}</td>
											</#list>					
										</tr>
										
										<#list 0..genotypeComparaScores?size-1 as rowCount>
											<tr>
												<td style="font-size:11px;padding:1px;width:150px;">
													<b>${rowCount+1}:</b> <@nodeHashHrefLimit nodeHash=genotypes[rowCount].genotype limit=22/>
												</td>
											
												<#list 0..genotypeComparaScores[rowCount]?size-1 as colCount>
													<#if (colCount < rowCount)>
														<td style="background-color:#ffEEEE;font-size:11px;width:40px;padding:1px;">${genotypeComparaScores[rowCount][colCount]}</td>
													<#elseif (colCount > rowCount)>
														<td style="background-color:#EEFFEE;font-size:11px;width:40px;padding:1px;">${genotypeComparaScores[rowCount][colCount]}</td>
													<#else>
														<td style="width:40px;font-size:11px;padding:1px;background-color:#aaaaaa;"></td>
													</#if>
												</#list>
											</tr>
										</#list>
									</table>
								</td>
							</tr>
						</table>
						<a href="#" onclick="toggleTable('genotypeAnnotations','${hostname}');return false;">
							<div class="nodeDetailBox"><img id="genotypeAnnotations_image" src="/OBDUI/images/plus-box.gif" class="expandoImage"/>
								Genotype Annotations				
							</div>
						</a>
						<table class="statementTableWrapper">
							<tr>
								<td id="genotypeAnnotations" style="display:none;">
									<table class="annotationStatementTable">
										<#list genotypes as genotypeHash>
											<#if genotypeHash.annotationStatements?exists>
												<fieldset style="font-size:12px;border:1px dotted grey;">
													<legend style="font-size:14px;">
														<@nodeHashHref nodeHash=genotypeHash.genotype/>influences:
													</legend>
													<#list genotypeHash.annotationStatements as statement>
														&bull;<@nodeHashHref nodeHash=statement.object/><br/>
													</#list>
												</fieldset>
												<br/>
											</#if>
										</#list>
									</table>
								</td>
							</tr>
						</table>
					</#if>
				</td>
			</tr>
		</table>
		<@footer/>
	</body>
</html>

<#macro geneMetaData metaPairs>
	<table class="mdInfo">
		<#list metaPairs as metaPair>
			<tr>
				<th>Organism:</th>
				<td>Get
			</tr>
			<tr>
				<th>DB Xrefs:</th>
				<td>List</td>
			</tr>
		</#list>
	</table>
</#macro>
