<#include "PageMacros.ftl"> 
<head>

  <title>
   OBD Search
  </title>

  <link rel="stylesheet"
        title="standard"
        type="text/css"
        href="http://toy.lbl.gov:9012/amigo2/amigo2.css" />

  <link rel="stylesheet"
        title="standard"
        type="text/css"
        href="http://toy.lbl.gov:9012/amigo2/bbop/sortabletable.css" />
        
          <link rel="stylesheet"
        title="standard"
        type="text/css"
        href="http://toy.lbl.gov:9012/amigo2/bbop/progress/tag.css" />
 
        
  <@javascriptTags/>
 
 </head>

  <body onload="AmiGO2_Init('http://toy.lbl.gov:9012/amigo2',
                            '${baseUri}',
                            'AmiGO2_User_ID');"
        onunload="">
   <div class="page_header">
     <span class="page_logo">[ logo]</span>

     <span class="page_title">Welcome!</span>
   </div>
   <@searchForm/>
   </div>
   <div class="container">
     <div id="replace_me">replace me</div>
   </div>
   
 
  </body>

</html>
