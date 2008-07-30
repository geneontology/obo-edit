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
					<@phat id=encodedId/>
					<hr class="divider"/>
					<@altViews nodeViews=nodeViews/>
				</td>
				<td id="vertical_divider">
				</td>
				<td id="content_container">
					<h2>Gene ${geneId}<#if geneLabel?exists>: ${geneLabel}</#if></h2><br/>
					<#if (genotypes?size > 0)>
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
													<a href="#genotype_${rowCount}" onclick="openTable('genotypeAnnotations','${hostname}');" title="${genotypes[rowCount].genotype.label}">
														<#if (genotypes[rowCount].genotype.label?length>20)>
															${genotypes[rowCount].genotype.label?substring(0,20)}...
														<#else>
															${genotypes[rowCount].genotype.label}
														</#if>
													</a>
												</th>
												<#list 0..genotypeComparaScores[rowCount]?size-1 as colCount>
													<#if (genotypeComparaScores[rowCount][colCount] != -1)>
														<td id="td_${rowCount}_${colCount}" onmouseover="toggleColors('on',${rowCount},${colCount},<#if (colCount<rowCount)>'basicScore','contentRatioScore'<#else>'contentRatioScore','basicScore'</#if>);" onmouseout="toggleColors('off',${rowCount},${colCount},<#if (colCount<rowCount)>'basicScore','contentRatioScore'<#else>'contentRatioScore','basicScore'</#if>);" class="<#if (colCount < rowCount)>basicScore<#else>contentRatioScore</#if>">
															<a style="color:#000088;" href="/${contextName}/${dataSource}/html/similarityPair/${genotypes[rowCount].genotype.encodedId}+${genotypes[colCount].genotype.encodedId}">${genotypeComparaScores[rowCount][colCount]}</a>
														</td>
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
										${annotationStatementCount} total annotation statements.<br/><br/>
										<#list 0..genotypes?size-1 as genotypeNum>
											<a id="genotype_${genotypeNum}"/>
											
												<fieldset style="font-size:12px;border:1px dotted grey;">
													
													<legend style="font-size:14px;">
														${genotypeNum+1}: <@nodeHashHref nodeHash=genotypes[genotypeNum].genotype/>influences:
													</legend>
													<#if genotypes[genotypeNum].annotationStatements?exists>
														<#list genotypes[genotypeNum].annotationStatements as statement>
															&bull;<@nodeHashHref nodeHash=statement.object/><br/>
														</#list>
													<#else>
														No Genotype Annotations.
													</#if>
												</fieldset>
												<br/>
										</#list>
									</table>
								</td>
							</tr>
						</table>
					<#else>
						No Genotypes found.
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
