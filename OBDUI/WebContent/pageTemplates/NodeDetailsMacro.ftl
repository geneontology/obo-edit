<#macro nodeDetailsTable>
 	<script type="text/javascript" src="/OBDUI/js/table-toggle.js"></script>
	<a href="#" onclick="toggleTable('statementsAbout');return false;">
		<div class="nodeDetailBox"><img id="statementsAbout_image" src="/OBDUI/images/plus-box.gif" class="expandoImage"/>Statements About This Node</div>
	</a>
	<table class="statementTableWrapper">
		<tr>
			<td id="statementsAbout" style="display:none;">
				<#if aboutStatements?exists>
					<table class="otherFormats">
						<tr>
							<th>This data in other formats:</th>
							<#list ["obdxml","owl","obo","json"] as format>
							<td>
								<@otherFormatLink contextName=contextName dataSource=dataSource format=format view="nodes" id=node.getId() statementType="about"/>
							</td>
							</#list>
						</tr>
					</table>
					<table class="annotationStatementTable">
						<#list aboutStatements as statement>
						<tr>
							<th>
								&bull;&nbsp;<span class="fixed_width">[${statement.entailment}]</span>		
							</th>
					 		<td>
					 			${statement.statement}
					 		</td>
					 	</tr>
					 	</#list>
					 </table>
				<#else>
					None
				</#if>
				
			</td>
		</tr>
	</table>
	<a href="#" onclick="toggleTable('statementsTo');return false;">
		<div class="nodeDetailBox"><img id="statementsTo_image" src="/OBDUI/images/plus-box.gif" class="expandoImage"/>Statements To This Node	</div>
	</a>
	<table class="statementTableWrapper">
		<tr>
			<td id="statementsTo" style="display:none;">
			
				<#if toStatements?exists>
					<table class="otherFormats">
						<tr>
							<th>This data in other formats:</th>
							<#list ["obdxml","owl","obo","json"] as format>
							<td>
								<@otherFormatLink contextName=contextName dataSource=dataSource format=format view="nodes" id=node.getId() statementType="to"/>
							</td>
							</#list>
						</tr>
					</table>
					<table class="annotationStatementTable">
						<#list toStatements as statement>
							<tr>
								<th>
									&bull;&nbsp;<span class="fixed_width">[${statement.entailment}]</span>		
								</th>
						 		<td>
						 			${statement.statement}
						 		</td>
						 	</tr>
						</#list>
					</table>
				<#else>
					None
				</#if>
			</td>
		</tr>
	</table>
	<a href="#" onclick="toggleTable('annotationStatements');return false;">
		<div class="nodeDetailBox"><img id="annotationStatements_image" src="/OBDUI/images/plus-box.gif" class="expandoImage"/>Class Annotations</div>
	</a>
	<table class="statementTableWrapper">
		<tr>
			<td id="annotationStatements" style="display:none;">
				<#if annotationStatements?exists>
					<table class="otherFormats">
						<tr>
							<th>This data in other formats:</th>
							<#list ["obdxml","owl","obo","json"] as format>
							<td>
								<@otherFormatLink contextName=contextName dataSource=dataSource format=format view="nodes" id=node.getId() statementType="annotations"/>
							</td>
							</#list>
						</tr>
					</table>
						<table class="annotationStatementTable">
							<#list annotationStatements as statementMap>
							<tr>
								<th rowspan="3">
									&bull;
								</th>
								<td>
									${statementMap.statement}
								</td>
							</tr>
							<tr>
								<td>
									<hr class="li_divider"/>
								</td>
							</tr>
							<tr>
								<td>
									Provenance:
									<#if statementMap.provenance?has_content>
										<#list statementMap.provenance as p>
											${p},  
										</#list>
									<#else>
										None.
									</#if>&nbsp;&nbsp;
									Source:
									<#if statementMap.assigned_by?has_content>
										<#list statementMap.assigned_by as c>
											${c} 
										</#list>
									<#else>
										None.
									</#if>
								</td>
							</tr>
							<tr>
								<td colspan="2" height="10"></td>
							</tr>
							</#list>
						</table>
					
					
				<#else>
					No Class Annotations.
				</#if>
			</td>
		</tr>
	</table>
</#macro>		


<#macro otherFormatLink contextName dataSource format view id statementType>
	<a href="/${contextName}/${dataSource}/${format}/${view}/${id}/statements/${statementType}">${format}</a>
</#macro>		



<#macro phat id>
	<a style="font-size:12px;" href="/${contextName}/${dataSource}/html/nodes/${id}/blast">Find similar nodes&nbsp;&raquo;</a>
</#macro>

<#macro nodeMetaData>
	<span style="font-size:12px;">Node MetaData</span>
	<br/>
	<span style="font-size:11px;">
	<#if node.getSourceId()?exists>
		&nbsp;&bull;&nbsp;<b>Source:</b> ${node.getSourceId()}
	</#if>
	</span>
</#macro>