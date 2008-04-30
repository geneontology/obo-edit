<#include "NodeDetailsMacro.ftl">
<#include "PageMacros.ftl">


<h2 style="padding-bottom:0px;padding-left:10px;font-size:18px;color:#3A3C72;font-family:Arial;margin-top:4px;margin-bottom:2px;">Class Annotations</h2>
<table style="width:100%;padding-top:0px;">
	<tr>
		<td>
			<#if annotationStatements?exists>
				<table style="width:100%;background-color:#888888;border-collapse:collapse;border-spacing:0px;">
					<tr>
						<th style="width:20%;font-family:Arial;font-weight:normal;font-size:12px;background-color:#cccccc;border:1px solid #aaaaaa;">This data in other formats:</th>
						<#list ["obdxml","owl","obo","json"] as format>
						<td style="width:20%;text-align:center;font-family:Arial;font-size:12px;background-color:#efefef;border:1px solid #aaaaaa;">
							<@otherFormatLink contextName=contextName dataSource=dataSource format=format view="nodes" id=node.getId() statementType="annotations"/>
						</td>
						</#list>
					</tr>
				</table>
				<table style="width:100%;">
					<#list annotationStatements as statementMap>
					<tr>
						<th rowspan="3" style="vertical-align:top;padding-top:0px;padding-left:4px;padding-right:4px;font-family:Arial;font-size:20px;color:#000066;">
							&bull;
						</th>
						<td style="font-family:Arial;font-size:12px;padding-top:4px;">
							<@labelhref target=statementMap.statement.sourceHref label=statementMap.statement.sourceLabel/> <@labelhref target=statementMap.statement.relationHref label=statementMap.statement.relationLabel/> <@labelhref target=statementMap.statement.targetHref label=statementMap.statement.targetLabel/>
						</td>
					</tr>
					<tr>
						<td>
							<hr style="padding:0px;margin:0px;height:0px;border-top-style:dotted;border-top-color:#afafaf;border-collapse:collapse;border-bottom-style:none;"/>
						</td>
					</tr>
					<tr>
						<td style="font-family:Arial;font-size:12px;">
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


