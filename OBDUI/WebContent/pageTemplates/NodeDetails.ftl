<#include "PageMacros.ftl"> 
<html>

        
    <@genTitle pageName="Node Details"/>
   	
	<body>
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
					<h2>${node.getId()}<#if node.getLabel()?has_content>: ${node.getLabel()}</#if></h2>
					<#if node.getSourceId()?has_content><b>Source:</b> ${node.getSourceId()}</#if>
					<br/>
					<br/>
					<a href="#" onclick="toggleTable('statementsAbout');return false;"><div class="nodeDetailBox" id="aboutStatements">
						Statements About This Node
					</div></a>
					<table class="statementTableWrapper">
						<tr>
							<td id="statementsAbout" style="display:none;">
								
								<#if aboutStatements?exists>
									<ul class="annotationStatement">
									<#list aboutStatements as statement>
										<li>[${statement.entailment}] ${statement.statement}</li>
									</#list>
								<#else>
									None
								</#if>
								
							</td>
						</tr>
					</table>
					<div class="nodeDetailBox" id="toStatements">
						Statements To This Node
					</div>
					<a href="#" onclick="toggleTable('annotationStatements');return false;"><div class="nodeDetailBox">
						Class Annotations
					</div></a>
					<table class="statementTableWrapper">
						<tr>
							<td id="annotationStatements" style="display:none;">
								
									<#if annotationStatements?exists>
										<ul class="annotationStatement">
										
										<#list annotationStatements as statementMap>
											<li>		
											<#list statementMap.statement as statement>
												${statement}
											</#list><hr class="li_divider"/>
											<span class="li_lowerspan">Provenance:
											<#if statementMap.provenance?has_content>
												<#list statementMap.provenance as p>
													${p} 
												</#list>
											<#else>
												None.
											</#if><br/>
											Source:
											<#if statementMap.assigned_by?has_content>
												<#list statementMap.assigned_by as c>
													${c} 
												</#list>
											<#else>
												None.
											</#if><br/>
											</span>
											
											 
											</li>
										</#list>
										</ul>
									</#if>
								
							</td>
						</tr>
					</table>
				</td>
			</tr>
		</table>
		<@footer/>
	</body>
</html>
