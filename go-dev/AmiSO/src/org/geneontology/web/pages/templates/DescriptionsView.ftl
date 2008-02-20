<#include "PageMacros.ftl"> 

<@page title="Compositional Descriptions">

<#macro genusDifferentiaCols id desc>
<!-- Genus-differentia (cross-product) definition -->
<#if desc.isGenusDifferentia()>
 <tr>
   <td> <@nodePageLink id=id/> </td>
   <td> <@nodePageLink id="${desc.getGenus().getNodeId()}"/></td>
   <#list desc.getDifferentiaArguments() as diff>
    <td><@nodePageLink id="${diff.getRelationId()}"/><td>
    <#-- simplifying assumption: we assume the class in the differentium is named -->
    <td><@nodePageLink id="${diff.getFirstArgument().getNodeId()}"/></td>
   </#list>
 </tr>
</#if>
</#macro>

<@section title="Descriptions">
<table>
 <#list graph.getNodes() as node>
 <#if graph.getCompositionalDescription(id)?exists>
 
   <@genusDifferentiaCols id=node.getId() desc=graph.getCompositionalDescription(node.getId())/>
 
 </#if>
</#list>
</table>
 </@section>
 <@exportLinks path="descriptions"/>
</@page>
