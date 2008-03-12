<#include "PageMacros.ftl"> 

<@page title="Phenotype Comparison Matrix for: ${focusLabel}">
<h1>
<#if annotGraph.getNode(focusId)?exists>
${annotGraph.getNode(focusId).getLabel()!""}
</#if>
</h1>

<#--

<@section title="Homolset annotation summary">
<div class="block" id="info">
species annotated:
<ul>
<#list homolview.getSpeciesIdsAnnotatedToSet() as n>
 <li>
  <@nodePageLinkNode node="${n}"/>
 </li>
</#list>
</ul>
annotation sources in set:
<ul>
<#list homolview.getSourceIdsAnnotatedToSet() as n>
 <li>
  <@nodePageLinkNode node="${n}"/>
 </li>
</#list>
</ul>

</div>

</@section>

-->

<@section title="Details">
 <table border="1">
  <tr>
   <th></th>
   <th></th>
   <th></th>
   <#list homolview.getNodeSets() as hset>
    <th class="node">
     <#list hset.getNodes() as n>
      <@href id="${n.getId()}" label="${n.getLabel()!n.getId()}"/>
     </#list>
    </th>
   </#list>
  </tr>
  <tr>
   <th>Species</th>
   <th>Source</th>
   <th>Genotype</th>
   <#list homolview.getNodeSets() as hset>
    <#assign hsetId>${hset.genId()!''}</#assign>
    <th class="node">
     <a href="#${hsetId}">detail</a>
     </th>
   </#list>
  </tr>
  <#list homolview.getAllSpeciesIds() as speciesId>
  <#list homolview.getAnnotatedEntitiesBySpeciesId(speciesId) as ae>
  <tr>
   <td class="node">
      ${homolview.getSpeciesId(ae.getId())!''}
    </td>
   <td class="node">
     ${ae.getSourceId()!''}
   </td>
   <td class="node">
       <@href id="${ae.getId()}" label="${ae.getLabel()!ae.getId()}"/>
   </td>
   <#list homolview.getNodeSets() as hset>
   <td>
    <#if homolview.isAnnotatedToNodeSet(ae,hset)>
     YES
    <#else>
     NO
    </#if>
   </td>
   </#list>
  </tr>
  </#list> <#-- annotated entities -->
  </#list> <#-- species -->
 </table>
 </section>
</@section>

<@section title="All annotations">

<#list homolview.getNodeSets() as hset>
 <div class="homolblock">
     <a name="${hset.genId()}"/>
     <ul>
     <#list hset.getNodes() as n>
      <li><@href id="${n.getId()}" label="${n.getLabel()!n.getId()}"/>
       -- ${n.getId()}
      </li>
     </#list>
     </ul>
    
    <table>
    <#list homolview.getAnnotStatementsByNodeSet(hset) as annotStatement>
     <@statementRow statement=annotStatement />
    </#list>
    </table>
    
   <!-- by quality -->
     <#--
    <#list homolview.getQualityIdsForAnnotationClass(hset) as qid>
   
    </#list>
   -->
 </div> 
</#list>


</@section>

</@page>
