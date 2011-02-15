<#include "PageMacros.ftl"> 

<@page title="Compositional Descriptions">

<#macro genusDifferentiaCols id desc>
<!-- Genus-differentia (cross-product) definition -->
<#if desc.isGenusDifferentia()>
 <tr>
   <td> <@nodePageLink id=id/> </td>
   <td><i>a</i></td>
   <td colspan="2"> <@nodePageLink id="${desc.getGenus().getNodeId()}"/></td>
 </tr>
 <#list desc.getDifferentiaArguments() as diff>
 <tr>
   <td> </td>
   <td>
   <#if diff_index==0>
    <i>that</i>
   <#else>
    <i>and</i>
   </#if>
   </td>
   <td><@nodePageLink id="${diff.getRelationId()}"/><td>
   <#-- simplifying assumption: we assume the class in the differentium is named -->
   <td><@nodePageLink id="${diff.getFirstArgument().getNodeId()}"/></td>
 </tr>
 </#list>
 <tr>
 </tr>
</#if>
</#macro>

<@section title="Descriptions">
<table>
 <tr>
  <th>Class</th>
  <th></th>
  <th>Definition</th>
 </tr>
 <#list graph.getNodes() as node>
 <#if graph.getCompositionalDescription(id)?exists>
 
   <@genusDifferentiaCols id=node.getId() desc=graph.getCompositionalDescription(node.getId())/>

 </#if>
</#list>
</table>
 </@section>
 <@exportLinks path="descriptions"/>
</@page>
