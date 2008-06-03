<#macro nodeHashHref nodeHash target='blank'>
	<#if nodeHash.isComposed?exists>
		<#if target != 'blank'>
			<@labelDecompose node=nodeHash.composedNode target=target/>
		<#else>
			<@labelDecompose node=nodeHash.composedNode target=nodeHash.href/>
		</#if>
	<#else>
		<#if target != 'blank'>
			<@labelhref target=target label=nodeHash.label/>
		<#else>
			<@labelhref target=nodeHash.href label=nodeHash.label/>
		</#if>
	</#if>
</#macro>

<#macro nodeHashHrefLimit nodeHash limit target='blank'>
	<#if nodeHash.isComposed?exists>
		<#if target != 'blank'>
			<@labelDecomposelimit node=nodeHash.composedNode target=target limit=limit/>
		<#else>
			<@labelDecomposelimit node=nodeHash.composedNode target=nodeHash.href limit=limit/>
		</#if>
	<#else>
		<#if target != 'blank'>
			<@labelhreflimit target=target label=nodeHash.label limit=limit/>
		<#else>
			<@labelhreflimit target=nodeHash.href label=nodeHash.label limit=limit/>
		</#if>
	</#if>
</#macro>

<#macro labelhref target='blank' label='blank'>
	<#if target=='blank'>
		${label}
	<#else>
		<a href="${target}" style="text-decoration:none;">${label}</a>
	</#if>
</#macro>

<#macro labelhreflimit limit target='blank' label='blank' >
	<#if ((label?length) > limit)>
		<#if target=='blank'>
			${label?substring(0,limit)}...
		<#else>
			<a href="${target}" title="${label}" style="text-decoration:none;">${label?substring(0,limit)}...</a>
		</#if>
	<#else>
		<#if target=='blank'>
			${label}
		<#else>
			<a href="${target}" style="text-decoration:none;">${label}</a>
		</#if>
	
	</#if>
	
</#macro>

<#macro labelabsolutehref target='blank' label='blank'>
	<#if target=='blank'>
		${label}
	<#else>
		<a href="http://${hostname}${target}" style="text-decoration:none;">${label}</a>
	</#if>
</#macro>

<#macro searchForm>
 <div id="searchbar">
 	 <script type="text/javascript" src="/${contextName}/js/search.js">
  	</script>
  	
	<form id="search_form" action="" method="get">
		Search <select id="search_target_data" style="font-size:10px;">
					<option value="nodes">nodes</option>
					<option value="statements">statements</option>
				</select> for:<br/>
		<#if dataSource?exists>
       		<input id="search_term" name="search_term" size="15"/>&nbsp;&bull;&nbsp;<input name="button" type="button"  onclick="nodeSearch('${contextName}','${dataSource}')" value="Go" />
       	<#else>
       		<input id="search_term" name="search_term" size="15"/>&nbsp;&bull;&nbsp;<input name="button" type="button"  onclick="nodeSearch('${contextName}','obdPhenotypeAll2008')" value="Go" />
       	</#if>
       	<!-- This is a hardcode of multiple datasources until the ui better handles multiple data sources -->
        <input type="hidden" name="dataSource" id="dataSource" value="obdPhenotypeAll2008"/>
     </form>
 </div>
</#macro>

<#macro labelDecompose node target="none">
	<#if target != "none">
		<a href="${target}">
	</#if>
	<#if node.subjectLabel?exists>
		${node.subjectLabel}
	</#if>
	<#if node.relationLabel?exists>
		<span style="color:#ff0000;">${node.relationLabel}</span>
	</#if>
	<#list node.args as arg>
		<#if arg.relationLabel?exists>
			<span style="color:#ff0000;">${arg.relationLabel}</span>
		</#if>
		<#if arg.targetIsComplex?exists>
			(<@labelDecompose node=arg.composedClass/>)
		<#else>
			${arg.targetLabel}
		</#if>
	</#list>
	<#if target != "none">
		</a>
	</#if>
</#macro>

<#macro labelDecomposeLimit limit node target="none" >
	<#if target != "none">
		<a href="${target}">
	</#if>
	<#if node.subjectLabel?exists>
		${node.subjectLabel}
	</#if>
	<#if node.relationLabel?exists>
		<span style="color:#ff0000;">${node.relationLabel?substring(0,limit)}</span>
	</#if>
	<#list node.args as arg>
		<#if arg.relationLabel?exists>
			<span style="color:#ff0000;">${arg.relationLabel?substring(0,limit)}</span>
		</#if>
		<#if arg.targetIsComplex?exists>
			(<@labelDecompose node=arg.composedClass limit=limit/>)
		<#else>
			${arg.targetLabel?substring(0,limit)}
		</#if>
	</#list>
	<#if target != "none">
		</a>
	</#if>
</#macro>


<#macro mappedPathsList>
	Mapped Paths:
	<table class="std_table">
	<#list pathMaps as path>
		<tr>
			<th>${path.className}</th><td>${path.mappedPath}</td>
		</tr>
	</#list>
	</table>
</#macro>
<#macro dataSourceList>
	Available Data Sources:
	<ul>
	<#list dataSources as dataSource>
		<li>${dataSource}</li>
	</#list>
	</ul>
	
	<br/>
	Configuration Messages:
	<ul>
	<#list configurationMessages as message>
		<li>${message}</li>
	</#list>
	</ul>
</#macro>

<#macro genTitle pageName>
		
		<title>
			${pageName}
		</title>
	
</#macro>

<#macro stylize>
	<link rel="stylesheet" type="text/css" href="/OBDUI/css/obd-main.css"/>
</#macro>

<#macro header>
	<div class="new_page_header">
		<a href="/OBDUI/" class="new_page_logo">OBD</a>
		<div class="full_div_blue"></div>
		<div class="menu_bar">
			<div class="menu_items">
				OBD Home&nbsp;&raquo;&nbsp;Node Detail
			</div>
			<div class="contact">
				Contact Us&nbsp;&raquo; 
			</div>
		</div>
		<div class="full_div_blue"></div>	
	</div>
</#macro>

<#macro footer>
	<div id="footer">
		<div class="full_div_blue">
		</div>
		<div id="footer_content">
			Page Last Updated March, 2008&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&bull;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&copy;&nbsp;Berkeley Bioinformatics and Ontologies Group and the National Center for Biomedical Ontologies 
		</div>
	</div>
</#macro>

<#macro border>
  <table border=4 cellspacing=0 cellpadding=4><tr><td>
    <#nested>
  </tr></td></table>
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
<a href="/${contextPath}/${dataSource}/${format}/node/${id}">${label}</a>
</#macro>

<#macro hrefSearch term format="html" label="${term}" dataSource="obdPhenotypeAll" contextPath="OBDUI">
<a href="/${contextPath}/${dataSource}/${format}/search/exact/${term}">${label}</a>
</#macro>

<#macro usecase id label="Use Case ${id}" contextPath="OBDUI">
<a href="/${contextPath}/usecases/${id}.html">Use Case ${id}</a>
</#macro>


<#macro resturl url>
<a href="${url}">${url}</a>
</#macro>

<#macro obdurl path format="html" dataSource="obdPhenotypeAll" contextPath="OBDUI">
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
