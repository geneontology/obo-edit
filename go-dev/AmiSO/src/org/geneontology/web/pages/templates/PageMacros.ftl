
<#macro border>
  <table border=4 cellspacing=0 cellpadding=4><tr><td>
    <#nested>
  </tr></td></table>
</#macro>

<#macro searchForm>
   <div class="search">
     <form id="search_form">
        <input id="search_term" name="search_term"/>
      <input name="button"
            type="button"
            onclick="nodeSearch()"
            value="Full Results" />
     </form>
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
          src="http://toy.lbl.gov:9012/amigo2/obd/model.js">
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

<#macro page title="OBD">
 <html>
<head>
   <meta http-equiv="Content-Type"
      content="text/html; charset=iso-8859-1" />
<meta name="description"
      content="OBD: A Database store of biomedical annotations" />

<@javascriptTags/>
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
  <h1>
    ${title}
  </h1>
  <div class="content">
    <div id="replace_me">
     <#nested>
    </div>
  </div>
 </body>
</html>
</#macro>
