<#include "PageMacros.ftl"> 
<#include "NodeDetailsMacro.ftl"> 
<html>
	<head>
		<@stylize/>
    	<@genTitle pageName="Similarity Pair Details"/>
    	
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
					<h2>${node1.label} <span style="font-weight:normal;">vs</span> ${node2.label}</h2><br/>
					
					<table class="intersectionPairTable">
						<tr>
							<th class="scoringSummary" colspan="2">
								<b>Scoring Summary:</b>
								Basic Similarity Score: <i>${basicSimilarityScore}</i>
								&nbsp;&nbsp;&nbsp;&nbsp;
								Information Content Ratio: <i>${contentRatioScore}</i>
							</th>
						</tr>
						<tr>
							<th class="set1"><@nodeHashHref nodeHash=node1/> Statements (${intersectionNodes?size + set1unique?size})</th>
							<th class="set2"><@nodeHashHref nodeHash=node2/> statements (${intersectionNodes?size + set2unique?size})</th>
						</tr>
						<tr>
							<td class="common" colspan="2">
								<table class="center">
									<tr>
										<td>
											<span style="width:100%;font-size:10px;color:#888888;">${intersectionNodes?size} Statements in Common</span><br/>
											<#if (intersectionNodes?size>0)>
												<@listNodes nodeList=intersectionNodes showScore='true'/>
											<#else>
												None
											</#if>
										</td>
									</tr>
								</table>
							</td>
						</tr>
						<tr>
						
							<td class="set1unique">
								<@listNodes nodeList=set1unique/>	
							</td>
							<td class="set2unique">
								<@listNodes nodeList=set2unique/>
							</td>
							
						</tr>
					</table>
					
				</td>
			</tr>
		</table>
		<@footer/>
	</body>
</html>

<#macro listNodes nodeList showScore='false'>
	<table class="nodeList">
	<#list nodeList as node>
		<tr>
			<td style="vertical-align:top;">&bull;</td>
			<#if (showScore != 'false')>
				<td style="width:50px;vertical-align:top;">${node.contentScore}</td>
			</#if>
			<td><@nodeHashHref nodeHash=node/></td>
		</tr>
	</#list>
	</table>
</#macro>
