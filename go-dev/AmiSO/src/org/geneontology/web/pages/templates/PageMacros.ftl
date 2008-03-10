
<#macro border>
  <table border=4 cellspacing=0 cellpadding=4><tr><td>
    <#nested>
  </tr></td></table>
</#macro>

<#macro searchForm>
 <div id="searchbar">
      <form id="search_form">
        <input id="search_term" name="search_term"/>
      <input name="button"
            type="button"
            onclick="nodeSearch()"
            value="Search" />
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
          src="/js/ajax.js">
  </script>
  

  <script type="text/javascript"
          src="/js/obd-explorer.js">
  </script>
  
</#macro>

<#macro hrefNode id format="html" label="${id}">
<a href="/${format}/nodes/${id}">${label}</a>
</#macro>

<#macro hrefSearch term format="html" label="${term}">
<a href="/${format}/search/exact/${term}">${label}</a>
</#macro>

<#macro usecase id>
<a href="/usecases/${id}.html">Use Case ${id}</a>
</#macro>

<#macro resturl url>
<a href="${url}">${url}</a>
</#macro>

<#macro obdurl path format="json">
<a href="/${format}/${path}">/{format}/${path}</a> (see <a href="/html/${path}">HTML</a>)
</#macro>

<#macro section title>
<div class="section">
 <h2>${title}</h2>
 <#nested>
</div>
</#macro>

<#macro download id format>
<span class="format">
<a href="/${format}/entity/${id}">
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

<#macro export path format>
<span class="format">
<a href="/${format}/${path}">
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


<#macro href id label="">
 <span class="identifier">
  <a href="/view/entity/${id}">${label}</a>
 </span>
</#macro>

<#macro nodePageLink id>
<span class="identifier">
<a href="/view/entity/${id}">
<#if graph.getNode(id)?exists>
${graph.getNode(id).getLabel()!id}
<#else>
${id}
</#if>
</a>
</span>
</#macro>


<#macro nodePageLinkNode node>
<span class="identifier">
<a href="/view/entity/${node.getId()}">
<#if node.getLabel()?exists>
${node.getLabel()}
<#else>
${node.getId()}
</#if>
</a>
</span>
</#macro>

<#-- TODO: make configurable -->
<#macro nodePageLinkFull id>
<span class="identifier">
<a href="/view/entity/${id}">
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

<#macro page title="OBD">
 <html>
<head>
   <meta http-equiv="Content-Type"
      content="text/html; charset=iso-8859-1" />
<meta name="description"
      content="OBD: A Database store of biomedical annotations" />

<@javascriptTags/>

<link href="/css/formatting.css"
      media="all"
      rel="Stylesheet"
      type="text/css" />
      <#-- 
<link href="/css/main.css"
      media="all"
      rel="Stylesheet"
      type="text/css" />
<link href="/css/table.css"
      media="all"
      rel="Stylesheet"
      type="text/css" />
<link href="/css/menu.css"
      media="all"
      rel="Stylesheet"
      type="text/css" />
<link href="/css/detail.css"
      media="all"
      rel="Stylesheet"
      type="text/css" />
<link href="/css/list.css"
      media="all"
      rel="Stylesheet"
      type="text/css" />
      -->
<link rel="shortcut icon"
      href="/images/OBD.png"
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
