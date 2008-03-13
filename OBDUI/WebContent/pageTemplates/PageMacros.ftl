
<#macro border>
  <table border=4 cellspacing=0 cellpadding=4><tr><td>
    <#nested>
  </tr></td></table>
</#macro>

<#macro searchForm>
 <div id="searchbar">

      <form id="search_form">
      	<table cellspacing="5">
      		<tr>
      			<td>Search for</td><td>In:</td>
      		</tr>
      		<tr>
      			<td>
        			<input id="search_term" name="search_term"/>
        		</td>
        		<td>
        			<select id="dataSource" name="dataSource">
        				<#list dataSources as dataSource>
        					<option value="${dataSource}">${dataSource}</option>
        				</#list>
        			</select>
        		</td>
        	</tr>
        </table>
        
        <input name="button" type="button" onclick="nodeSearch('${contextName}')" value="Search" />
     </form>
 </div>
</#macro>

<#macro javascriptTags>
  
 <script type="text/javascript"
          src="http://toy.lbl.gov:9012/amigo2/external/Prototype/prototype.js">
  </script>

  <script type="text/javascript"
          src="http://toy.lbl.gov:9012/amigo2/amigo/store.js">
  </script>


  <script type="text/javascript"
          src="http://toy.lbl.gov:9012/amigo2/bbop/protocontrol.js">
  </script>

  <script type="text/javascript"
          src="http://toy.lbl.gov:9012/amigo2/bbop/timer.js">
  </script>

 <script type="text/javascript"
          src="http://toy.lbl.gov:9012/amigo2/bbop/progress/tag.js">
  </script>

  <script type="text/javascript"
          src="http://toy.lbl.gov:9012/amigo2/bbop/shadowsortfunctions.js">
  </script>


  <script type="text/javascript"
          src="http://toy.lbl.gov:9012/amigo2/bbop/sortabletable.js">
  </script>


  <script type="text/javascript"
          src="http://toy.lbl.gov:9012/amigo2/obd/handle.js">
  </script>

  
    <script type="text/javascript"
          src="/OBDUI/js/ajax.js">
  </script>
  

  <script type="text/javascript"
          src="/OBDUI/js/obd-explorer.js">
  </script>
  
</#macro>

<#macro hrefNode id format="html" label="${id}" dataSource="obdPhenotypeAll" contextPath="OBDUI">
<a href="/${contextPath}/${dataSource}/${format}/nodes/${id}">${label}</a>
</#macro>

<#macro hrefSearch term format="html" label="${term}" dataSource="obdPhenotypeAll" contextPath="OBDUI">
<a href="/${contextPath}/${dataSource}/${format}/search/exact/${term}">${label}</a>
</#macro>

<#macro usecase id contextPath="OBDUI">
<a href="/${contextPath}/usecases/${id}.html">Use Case ${id}</a>
</#macro>

<#macro resturl url>
<a href="${url}">${url}</a>
</#macro>

<#macro obdurl path format="json" dataSource="obdPhenotypeAll" contextPath="OBDUI">
<a href="/${contextPath}/${dataSource}/${format}/${path}">/{contextPath}/{dataSource}/{format}/${path}</a> (see <a href="/${contextPath}/${dataSource}/html/${path}">HTML</a>)
</#macro>

<#macro section title>
<div class="section">
 <h2>${title}</h2>
 <#nested>
</div>
</#macro>

<#macro download id format dataSource="obdPhenotypeAll" contextPath="OBDUI">
<span class="format">
<a href="/${contextPath}/${dataSource}/${format}/entity/${id}">
${format}
</a>
</span>
</#macro>


<#macro downloadLinks id>
[
<@download id="${id}" format="obo"/>
|
<@download id="${id}" format="owl"/>
|
<@download id="${id}" format="obdxml"/>
|
<@download id="${id}" format="json"/>
]
</#macro>

<#macro export path format dataSource="obdPhenotypeAll" contextPath="OBDUI">
<span class="format">
<a href="/${contextPath}/${dataSource}/${format}/${path}">
${format}
</a>
</span>
</#macro>

<#macro exportLinks path>
[
<@export path="${path}" format="obo"/>
|
<@export path="${path}" format="owl"/>
|
<@export path="${path}" format="obdxml"/>
|
<@export path="${path}" format="json"/>
]
</#macro>


<#macro href id label="" dataSource="obdPhenotypeAll" contextPath="OBDUI">
 <span class="identifier">
  <a href="/${contextPath}/${dataSource}/view/entity/${id}">${label}</a>
 </span>
</#macro>

<#macro nodePageLink id dataSource="obdPhenotypeAll" contextPath="OBDUI">
<span class="identifier">
<a href="/${contextPath}/${dataSource}/view/entity/${id}">
<#if graph.getNode(id)?exists>
${graph.getNode(id).getLabel()!id}
<#else>
${id}
</#if>
</a>
</span>
</#macro>


<#macro nodePageLinkNode node dataSource="obdPhenotypeAll" contextPath="OBDUI">
<span class="identifier">
<a href="/${contextPath}/${dataSource}/view/entity/${node.getId()}">
<#if node.getLabel()?exists>
${node.getLabel()}
<#else>
${node.getId()}
</#if>
</a>
</span>
</#macro>

<#-- TODO: make configurable -->
<#macro nodePageLinkFull id dataSource="obdPhenotypeAll" contextPath="OBDUI">
<span class="identifier">
<a href="/${contextPath}/${dataSource}/view/entity/${id}">
${id}
</a>
</span>
<#if graph.getNode(id)?exists>
${graph.getNode(id).getLabel()!""}
</#if>
</#macro>

<#macro statementRow statement>
 <tr>
   <td class="node">
    <#if statement.getNodeId() != focusId>  
    <i>
    <@nodePageLink id="${statement.getNodeId()}"/>
    </i>
    <#else>
     <@nodePageLink id="${statement.getNodeId()}"/>   
    </#if>
   </td>
   <td class="relation">
    <@nodePageLink id="${statement.getRelationId()}"/>
   </td>
  <td >
    <#if statement.getTargetId()?exists>
     <#if statement.getTargetId() != focusId>  
      <i>
      <@nodePageLink id="${statement.getTargetId()}"/>
      </i>
     <#else>
      <@nodePageLink id="${statement.getTargetId()}"/>
     </#if>
   </#if>
   </td>
   <td class="node">
    <#if statement.isInferred()>
     [implied]
    </#if>
   </td>
   <td class="source">
   <#if statement.getSourceId()?exists>
    <@nodePageLink id="${statement.getSourceId()}"/>
   </#if>
   </td>
   <td class="provenance">
    <#list statement.getSubStatements() as ss>
     <span class="tag">
      <@nodePageLink id="${ss.getRelationId()}"/>
     <span class="tag">
     </span>
      <@nodePageLink id="${ss.getTargetId()}"/>
      <#if ss.isLiteral()>
      <@nodePageLink id="${ss.getValue()}"/>
      </#if>
     </span>
    </#list>
   </td>
  </tr>
</#macro>

<#macro page title="OBD" contextPath="OBDUI">
 <html>
<head>
   <meta http-equiv="Content-Type"
      content="text/html; charset=iso-8859-1" />
<meta name="description"
      content="OBD: A Database store of biomedical annotations" />

<@javascriptTags/>

<link href="/${contextPath}/css/formatting.css"
      media="all"
      rel="Stylesheet"
      type="text/css" />
      <#-- 
<link href="/${contextPath}/css/main.css"
      media="all"
      rel="Stylesheet"
      type="text/css" />
<link href="/${contextPath}/css/table.css"
      media="all"
      rel="Stylesheet"
      type="text/css" />
<link href="/${contextPath}/css/menu.css"
      media="all"
      rel="Stylesheet"
      type="text/css" />
<link href="/${contextPath}/css/detail.css"
      media="all"
      rel="Stylesheet"
      type="text/css" />
<link href="/${contextPath}/css/list.css"
      media="all"
      rel="Stylesheet"
      type="text/css" />
      -->
<link rel="shortcut icon"
      href="/${contextPath}/images/OBD.png"
      type="image/x-icon" />

  <title>${title}</title>
</head>
<body>
 <div id="header">
  <span id="floatright">
    <a href="http://www.berkeleybop.org/obd" title="Home">Home</a>

    |
    <script type="text/javascript;e4x=1">
      obfuscate("berkeleybop.org","cjm","Contact");
    </script>
    |
    <a href="http://www.berkeleybop.org/obd/about.shtml" title="About">About</a>
  </span>
  <a class="image" href="http://www.berkeleybop.org/obd"
     title="OBD: Open Biomedical Database">
    <img src="http://www.berkeleybop.org/obd/images/OBD.png"
	 alt="OBD"
	 
 class="image" />
  </a>
 </div>
 
  <@searchForm/>
  <div class="block" id="info">
  <h1>
    ${title}
  </h1>
   <div class="content">
    <div id="replace_me">
     <#nested>
    </div>
   </div>
  </div>
 </body>
</html>
</#macro>
