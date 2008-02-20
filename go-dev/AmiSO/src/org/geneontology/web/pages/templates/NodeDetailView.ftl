<#include "PageMacros.ftl"> 

<@page title="Detail for: ${focusId}">
<h1>
<#if graph.getNode(focusId)?exists>
${graph.getNode(focusId).getLabel()!""}
</#if>
</h1>


<@section title="Class details">
<div class="block" id="info">
<nodePageLink id=${focusId}/>
<dl class="term-info">
 <dt>Synonyms</dt>
 <#list termview.getSynonymStatements(focusId) as statement>
  <dd class="syn">
  <#-- TODO: synonym scope and category -->
   ${statement.getValue()}
  </dd>
 </#list>

<dt>Text Definition</dt>
<#list termview.getTextualDefinitionStatements(focusId) as statement>
 <dd class="def">
  ${statement.getValue()}
 </dd>
</#list>

<dt>Subset</dt>
 <#list termview.getSubsetStatements(focusId) as statement>
  <dd class="subset">
    <@nodePageLink id="${statement.getTargetId()}"/>
  </dd>
 </#list>

<!-- Genus-differentia (cross-product) definition -->
<dt>compositional description</dt>
<#if graph.getCompositionalDescription(focusId)?exists>
<dd class="logical_definition">
 ${graph.getCompositionalDescription(focusId).toString()}
</dd>
</dt>
<#if graph.getCompositionalDescription(focusId).isGenusDifferentia()>
<dt>Genus</dt>

 <dd> <@nodePageLink id="${graph.getCompositionalDescription(focusId).getGenus().getNodeId()}"/>
 </dd>
</dt>
<dt>Differentia</dt>
<#list graph.getCompositionalDescription(focusId).getDifferentiaArguments() as diff>
<dd>
<@nodePageLink id="${diff.getRelationId()}"/>
<#-- simplifying assumption: we assume the class in the differentium is named -->
<@nodePageLink id="${diff.getFirstArgument().getNodeId()}"/>
</dd>
</#list>
</#if>

</div>
</#if>
</dl> <!-- end of class details -->

</@section>

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
  <#list graph.getAllLinkStatementsForNode(focusId) as s>
  <#-- only show plain links: not links that for N+S conditions -->
  <#if (!s.isIntersectionSemantics() && !s.isUnionSemantics())>
    <@statementRow statement=s/>
  </#if>
 </#list>
 </table>
 </@section>
 <@section title="Links to this class">
 
 <table>
  <#list graph.getAllLinkStatementsForTarget(focusId) as s>
  <#-- only show plain links: not links that for N+S conditions -->
  <#if !s.isIntersectionSemantics() && !s.isUnionSemantics()>
   <@statementRow statement=s/>
  </#if>
 </#list>
 </table>
</@section>


<@section title="Export">
<@downloadLinks id=focusId/>
</@section>

</@page>
