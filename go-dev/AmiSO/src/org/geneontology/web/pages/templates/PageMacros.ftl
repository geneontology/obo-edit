
<#macro border>
  <table border=4 cellspacing=0 cellpadding=4><tr><td>
    <#nested>
  </tr></td></table>
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

<#macro page title="OBD">
 <html>
<head>
   <meta http-equiv="Content-Type"
      content="text/html; charset=iso-8859-1" />
<meta name="description"
      content="OBD: A Database store of biomedical annotations" />

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
  <h1>
    ${title}
  </h1>
  <div class="content">
   <#nested>
  </div>
 </body>
</html>
</#macro>
