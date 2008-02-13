<#include "PageMacros.ftl"> 
<head>

  <title>
   OBD Search
  </title>

  <link rel="stylesheet"
        title="standard"
        type="text/css"
        href="http://toy.lbl.gov:9012/amigo2/amigo2.css" />

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

  <link rel="stylesheet"
        title="standard"
        type="text/css"
        href="http://toy.lbl.gov:9012/amigo2/bbop/progress/tag.css" />
  <script type="text/javascript"
          src="http://toy.lbl.gov:9012/amigo2/bbop/progress/tag.js">
  </script>

  <script type="text/javascript"
          src="http://toy.lbl.gov:9012/amigo2/bbop/shadowsortfunctions.js">
  </script>

  <link rel="stylesheet"
        title="standard"
        type="text/css"
        href="http://toy.lbl.gov:9012/amigo2/bbop/sortabletable.css" />
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

 </head>

  <body onload="AmiGO2_Init('http://toy.lbl.gov:9012/amigo2',
                            '${baseUri}',
                            'AmiGO2_User_ID');"
        onunload="">
   <div class="page_header">
     <span class="page_logo">[ logo]</span>

     <span class="page_title">Welcome!</span>
   </div>
   <div class="search">
     <form id="search_form">
        <input id="search_term" name="search_term"/>
      <input name="button"
            type="button"
            onclick="nodeSearch()"
            value="Full Results" />
      <input name="button"
            type="button"
            onclick="testSearch()"
            value="Test" />
    </form>
   </div>
   <div class="container">
     <div id="replace_me">replace me</div>
   </div>
   
 
  </body>

</html>
