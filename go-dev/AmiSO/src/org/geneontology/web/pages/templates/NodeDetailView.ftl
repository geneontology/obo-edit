<#include "PageMacros.ftl"> 

<@page title="Detail for: ${focusId}">

<#macro nodePageLink id>
<span class="identifier">
<a href="/view/entity/${id}">
${id}
</a>
</span>
<#if graph.getNode(id)?exists>
${graph.getNode(id).getLabel()!""}
</#if>
</#macro>

<@section title="Class details">
<nodePageLink id=${focusId}/>

<#macro download id format>
<span class="format">
<a href="/${format}/entity/${id}">
${format}
</a>
</span>
</#macro>

<div class="synonyms">
<#list termview.getSynonymStatements(focusId) as statement>
 <span class="synonym">
 <span class="tag">
  Synonym
 </span>
 <span class="value">
  ${statement.getValue()}
 </span>
</span>
</#list>
</div>

<div class="textual_definitions">
<#list termview.getTextualDefinitionStatements(focusId) as statement>
 <span class="definition">
 <span class="tag">
  Definition
 </span>
 <span class="value">
  ${statement.getValue()}
 </span>
</span>
</#list>
</div>

<div class="subsets">
<#list termview.getSubsetStatements(focusId) as statement>
 <span class="definition">
   <span class="tag">
  Subset
 </span>
 <span class="value">
    <@nodePageLink id="${statement.getTargetId()}"/>
  </span>
</span>
</#list>
</div>

</@section>

<!-- Genus-differentia (cross-product) definition -->
<div class="compositional_description">
<#if graph.getCompositionalDescription(focusId)?exists>
<@section title="Description">
<span class="test">
${graph.getCompositionalDescription(focusId).toString()}
</span>
<#if graph.getCompositionalDescription(focusId).isGenusDifferentia()>
<span class="genus">
Genus: <@nodePageLink id="${graph.getCompositionalDescription(focusId).getGenus()}"/>
</span>
<#list graph.getCompositionalDescription(focusId).getDifferentiaArguments() as diff>
<span class="differentium">
Differentium:
<@nodePageLink id="${diff.getRelationId()}"/>
<#-- simplifying assumption: we assume the class in the differentium is named -->
<@nodePageLink id="${diff.getFirstArgument().getNodeId()}"/>
</span>
</#list>
</#if>
</@section>
</#if>
</div>

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
 <@section title="Links from this class">
 
 <table>
  <#list graph.getAllLinkStatementsForNode(focusId) as statement>
  <#-- only show plain links: not links that for N+S conditions -->
  <#if (!statement.isIntersectionSemantics() && !statement.isUnionSemantics())>
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
  </#if>
 </#list>
 </table>
 </@section>
 <@section title="Links to this class">
 
 <table>
  <#list graph.getAllLinkStatementsForTarget(focusId) as statement>
  <#-- only show plain links: not links that for N+S conditions -->
  <#if !statement.isIntersectionSemantics() && !statement.isUnionSemantics()>
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
    </#if>
 </#list>
 </table>
</@section>

<@section title="Export">
[
<@download id="${focusId}" format="obo"/>
|
<@download id="${focusId}" format="owl"/>
|
<@download id="${focusId}" format="obdxml"/>
]
</@section>

</@page>
