<#include "PageMacros.ftl"> 
<#include "NodeDetailsMacro.ftl"> 
<html>
	<head>
		<@stylize/>
    	<@genTitle pageName="Node Details"/>
    	<script type="text/javascript" src="/OBDUI/js/table-toggle.js"></script>
		<script type="text/javascript">
			function toggleColors(way, row, column, primaryClass,secondaryClass){
				var primaryTd = document.getElementById(('td_' + row + '_' + column));
				var complementTd = document.getElementById(('td_' + column + '_' + row));
				var primaryTh = document.getElementById(('th_' + row ));
				var complementTh = document.getElementById(('th_' + column )); 
				if (way=='on'){
					primaryTd.className='primaryHighlight';
					complementTd.className='secondaryHighlight';
					primaryTh.className='primaryHighlight';
					complementTh.className='secondaryHighlight';
					
				} else {
					primaryTd.className=primaryClass;
					complementTd.className=secondaryClass;
					primaryTh.className='std';
					complementTh.className='std';
				}
			}
			
		</script>
		
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
									<table id="genotypeScores" class="genotypeScores">
										<tr>
											<th>Genotype #</th>
											<#list 1..genotypeComparaScores?size as num>
												<td><b>${num}</b></td>
											</#list>					
										</tr>
										<#list 0..genotypeComparaScores?size-1 as rowCount>
											<tr>
												<th id="th_${rowCount}" class="std">
													<b>${rowCount+1}:</b> 
													<a href="#genotype_${rowCount}" onclick="openTable('genotypeAnnotations','${hostname}');" name="${genotypes[rowCount].genotype.nodeLabel}">
														<#if (genotypes[rowCount].genotype.nodeLabel?length>22)>
															${genotypes[rowCount].genotype.nodeLabel?substring(0,22)}...
														<#else>
															${genotypes[rowCount].genotype.nodeLabel}
														</#if>
													</a>
												</th>
												<#list 0..genotypeComparaScores[rowCount]?size-1 as colCount>
													<#if (genotypeComparaScores[rowCount][colCount] != -1)>
														<td id="td_${rowCount}_${colCount}" onmouseover="toggleColors('on',${rowCount},${colCount},<#if (colCount<rowCount)>'basicScore','contentRatioScore'<#else>'contentRatioScore','basicScore'</#if>);" onmouseout="toggleColors('off',${rowCount},${colCount},<#if (colCount<rowCount)>'basicScore','contentRatioScore'<#else>'contentRatioScore','basicScore'</#if>);" class="<#if (colCount < rowCount)>basicScore<#else>contentRatioScore</#if>">${genotypeComparaScores[rowCount][colCount]}</td>
													<#else>
														<td class="blank"></td>
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
										<#list 0..genotypes?size-1 as genotypeNum>
											<a id="genotype_${genotypeNum}"/>
											<#if genotypes[genotypeNum].annotationStatements?exists>
												<fieldset style="font-size:12px;border:1px dotted grey;">
													
													<legend style="font-size:14px;">
														<@nodeHashHref nodeHash=genotypes[genotypeNum].genotype/>influences:
													</legend>
													<#list genotypes[genotypeNum].annotationStatements as statement>
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
