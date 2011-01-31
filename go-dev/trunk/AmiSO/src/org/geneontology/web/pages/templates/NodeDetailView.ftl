<#include "PageMacros.ftl"> 

<@page title="Detail for: ${focusId}">
<h1>
<#if graph.getNode(focusId)?exists>
${graph.getNode(focusId).getLabel()!""}
</#if>
</h1>


<@section title="Entity details">
<div class="block" id="info">
<nodePageLink id=${focusId}/>
<dl class="term-info">
 <#if termview.getSynonymStatements(focusId).size() != 0 > 
 <dt>Synonyms</dt>
 <#list termview.getSynonymStatements(focusId) as statement>
  <dd class="syn">
  <#-- TODO: synonym scope and category -->
   ${statement.getValue()}
  </dd>
 </#list>
 </#if>

<#if termview.getTextualDefinitionStatements(focusId).size() != 0 > 
 <dt>Text Definition</dt>
<#list termview.getTextualDefinitionStatements(focusId) as statement>
 <dd class="def">
  ${statement.getValue()}
 </dd>
</#list>
</#if>

<#if termview.getSubsetStatements(focusId).size() != 0 > 
<dt>Subset</dt>
 <#list termview.getSubsetStatements(focusId) as statement>
  <dd class="subset">
    <@nodePageLink id="${statement.getTargetId()}"/>
  </dd>
 </#list>
 </#if>
 

<#if graph.getCompositionalDescription(focusId)?exists>
<#-- TODO: non-G-D style -->
<#if graph.getCompositionalDescription(focusId).isGenusDifferentia()>
<!-- Genus-differentia (cross-product) definition -->
<dt>compositional description</dt>
<dd class="logical_definition">
 ${graph.getCompositionalDescription(focusId).toString()}
</dd>
</dt>
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

<@section title="Actions">
<div class="block" id="actions">
<a href="/html/nodes/${focusId}/blast">find similar</a>
</div>
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
 <@section title="Link Statements">
 
  <table>
  <#list graph.getAllLinkStatementsForNode(focusId) as s>
   <#-- only show plain links: not links that for N+S conditions -->
   <#if (!s.isInferred())>
    <#if (!s.isIntersectionSemantics() && !s.isUnionSemantics())>
     <@statementRow statement=s/>
    </#if>
   </#if>
  </#list>
 <#list graph.getAllLinkStatementsForNode(focusId) as s>
   <#-- only show plain links: not links that for N+S conditions -->
   <#if (s.isInferred())>
    <#if (!s.isIntersectionSemantics() && !s.isUnionSemantics())>
     <@statementRow statement=s/>
    </#if>
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
