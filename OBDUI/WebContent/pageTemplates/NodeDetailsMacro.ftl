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
								<#if statement.nscondition?exists>
									<span style="font-weight:normal;">&otimes;</span>&nbsp;<span class="fixed_width">[${statement.entailment}]</span>
								<#else>
									&bull;&nbsp;<span class="fixed_width">[${statement.entailment}]</span>
								</#if>		
							</th>
							
					 		<td>
					 			<@nodeHashHref nodeHash=statement.subject/>
					 			<@nodeHashHref nodeHash=statement.predicate/>
					 			<@nodeHashHref nodeHash=statement.object/>
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
									<@nodeHashHref nodeHash=statement.subject/>
					 				<@nodeHashHref nodeHash=statement.predicate/>
					 				<@nodeHashHref nodeHash=statement.object/>					 			
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
							<#list annotationStatements as statement>
							<tr>
								<th rowspan="3">
									&bull;
								</th>
								<td>
									<@nodeHashHref nodeHash=statement.subject/>
					 				<@nodeHashHref nodeHash=statement.predicate/>
					 				<@nodeHashHref nodeHash=statement.object/>
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
									<#if statement.provenance?has_content>
										<#list statement.provenance as p>
											${p},  
										</#list>
									<#else>
										None.
									</#if>&nbsp;&nbsp;
									Source:
									<#if statement.assigned_by?has_content>
										<#list statement.assigned_by as c>
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

<#macro altViews nodeViews>
	<#if nodeViews?exists>
		Alternate Views:<br/>
		<#list nodeViews as view>
			<span style="font-size:13px;">
				&nbsp;&bull;&nbsp;<a href="${view.href}">${view.view} view&nbsp;&raquo;</a><br/>
			</span>
		</#list>
		<hr class="divider"/>
	</#if>
</#macro>

<#macro nodeMetaData>
	To Do
</#macro>

