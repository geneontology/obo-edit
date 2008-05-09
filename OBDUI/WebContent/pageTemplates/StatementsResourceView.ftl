<#include "PageMacros.ftl"> 

<@page title="Statements for: ${focusId}">

<#macro nodePageLink id dataSource="obdPhenotypeAll" contextPath="OBDWS">
<span class="identifier">
<a href="/${contextPath}/${dataSource}/view/node/${id}">
${id}
</a>
</span>
<#if graph.getNode(id)?exists>
${graph.getNode(id).getLabel()!""}
</#if>
</#macro>

<@section title="Value Statements">
 <table>
 <#list graph.getLiteralStatements() as statement>
  <tr>
   <td class="node">
    <#if statement.getNodeId() != focusId>  
    <@nodePageLink id="${statement.getNodeId()}"/>
    </#if>
   </td>
    <td class="relation">
    <@nodePageLink id="${statement.getRelationId()}"/>
   </td>
  <td>
    <#if statement.getSValue()?exists>
     ${statement.getSValue()}
    </#if>
   </td>
   <td class="node">
    <#if statement.isInferred()>
     [implied]
    </#if>
   </td>
  </tr>
 </#list>
 </table>
 
 </@section>
 <@section title="Link Statements">
 
 <table>
  <#list graph.getLinkStatements() as statement>
  <tr>
   <td class="node">
    <#if statement.getNodeId() != focusId>  
    <@nodePageLink id="${statement.getNodeId()}"/>
    </#if>
   </td>
   <td class="relation">
    <@nodePageLink id="${statement.getRelationId()}"/>
   </td>
  <td >
    <#if statement.getTargetId()?exists>
     <#if statement.getTargetId() != focusId>  
      <@nodePageLink id="${statement.getTargetId()}"/>
     </#if>
   </#if>
   </td>
   <td class="node">
    <#if statement.isInferred()>
     [implied]
    </#if>
   </td>
  </tr>
 </#list>
 </table>
</@section>

</@page>
