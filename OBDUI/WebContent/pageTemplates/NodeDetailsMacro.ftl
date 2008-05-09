<#macro nodeDetailsTable>
 	<script type="text/javascript" src="/OBDUI/js/table-toggle.js"></script>
	<a href="#" onclick="toggleTable('statementsAbout','${hostname}');return false;">
		<div class="nodeDetailBox"><img id="statementsAbout_image" src="/OBDUI/images/plus-box.gif" class="expandoImage"/>
			<#if manyNodes?exists>
				Statements About These Nodes				
			<#else>
				Statements About This Node
			</#if>
		</div>
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
								<#if manyNodes?exists>
									<@otherFormatLink contextName=contextName dataSource=dataSource format=format view="nodes" id=encodedNodeString statementType="about"/>
								<#else>
									<@otherFormatLink contextName=contextName dataSource=dataSource format=format view="node" id=encodedId statementType="about"/>
								</#if>
							</td>
							</#list>
						</tr>
					</table>
					<table class="annotationStatementTable">
						<#list aboutStatements as statement>
						<tr>
							<th>
								<#if statement.statement.nscondition?exists>
									<span style="font-weight:normal;">&otimes;</span>&nbsp;<span class="fixed_width">[${statement.entailment}]</span>
								<#else>
									&bull;&nbsp;<span class="fixed_width">[${statement.entailment}]</span>
								</#if>		
							</th>
							
					 		<td>
					 			<@labelhref target=statement.statement.sourceHref label=statement.statement.sourceLabel/> 
					 			<@labelhref target=statement.statement.relationHref label=statement.statement.relationLabel/> 
					 			<@labelhref target=statement.statement.targetHref label=statement.statement.targetLabel/>
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
	<a href="#" onclick="toggleTable('statementsTo','${hostname}');return false;">
		<div class="nodeDetailBox"><img id="statementsTo_image" src="/OBDUI/images/plus-box.gif" class="expandoImage"/>
			<#if manyNodes?exists>
				Statements To These Nodes				
			<#else>
				Statements To This Node
			</#if>
		</div>
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
								<#if manyNodes?exists>
									<@otherFormatLink contextName=contextName dataSource=dataSource format=format view="nodes" id=encodedNodeString statementType="to"/>
								<#else>
									<@otherFormatLink contextName=contextName dataSource=dataSource format=format view="node" id=encodedId statementType="to"/>
								</#if>
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
						 			<@labelhref target=statement.statement.sourceHref label=statement.statement.sourceLabel/> 
						 			<@labelhref target=statement.statement.relationHref label=statement.statement.relationLabel/> 
						 			<@labelhref target=statement.statement.targetHref label=statement.statement.targetLabel/>
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
	<a href="#" onclick="toggleTable('annotationStatements','${hostname}');return false;">
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
								<#if manyNodes?exists>
									<@otherFormatLink contextName=contextName dataSource=dataSource format=format view="nodes" id=encodedNodeString statementType="annotations"/>
								<#else>
									<@otherFormatLink contextName=contextName dataSource=dataSource format=format view="node" id=encodedId statementType="annotations"/>
								</#if>
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
									<@labelhref target=statementMap.statement.sourceHref label=statementMap.statement.sourceLabel/> <@labelhref target=statementMap.statement.relationHref label=statementMap.statement.relationLabel/> <@labelhref target=statementMap.statement.targetHref label=statementMap.statement.targetLabel/>
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
	<a style="font-size:12px;" href="/${contextName}/${dataSource}/html/node/${id}/blast">Find similar nodes&nbsp;&raquo;</a>
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