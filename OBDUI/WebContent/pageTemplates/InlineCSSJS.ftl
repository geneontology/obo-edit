<#macro externalStyle>
	<style type="text/css">
		body {
			border-width: 0px;
			font-family: "Lucida Grande", Lucida, Verdana, sans-serif;
			font-size:12px;
		}
		
		h2 {
		    color:#000055;
		    padding-bottom:0px;
		    margin-bottom:2px;
		}
		a:link {
		    text-decoration: none; 
		}
		
		a:hover {
		    text-decoration: underline;
		}
		
		a:visited {
		    text-decoration: none;
		}
		
		a:visited:hover {
		    text-decoration: underline;
		}
		
		
		
		.new_page_header{
			width: 100%;
		}
		
		.new_page_logo{
		    width:800px;
		    color:#000077;
			font-size: 30px;
		}
		
		.full_div_blue{
		    width:100%;
		    background-color:#000077;
		    height:1px;
		}
		
		.menu_bar{
		    padding-top:2px;
		    padding-bottom:2px;
		    padding-right:5px;
		    padding-left:5px;
		    font-size:12px;
		    background-color:#efefef;
		}
		
		.menu_items {
		    float: left;   
		}
		
		.contact {
		    text-align:right;
		}
		
		#content_body {
		}
		
		
		#vertical_divider{
		    float:left;
		    
		}
		
		#content{
		    float: left;
		    padding:5px;
		}
		
		#footer{
		    clear:both;
		    font-size:10px;
		}
		
		#footer_content{
		    padding:3px;
		    text-align:center;
		}
		
		#searchbar{
		    font-size:12px;
		}
		
		table.std_table{
			border-style: none;
			border-spacing:0px ;
			border-color: #000077;
			background-color:#000077;
			border-collapse: collapse;
			font-size:11px;
		}
		
		table.std_table th{
		    border-width: 1px;
			padding: 2px;
			border-style: inset;
			border-color: #cccccc;
			background-color:#fdfdfd;
			font-weight: normal;
			text-align: left;
		}
		
		table.std_table td{
		    border-width: 1px;
			padding: 1px;
			border-style: inset;
			border-color: #cccccc;
			background-color: #efefef;
			text-align:left;
			padding:2px;
		}
		
		table.threecol {
		    width:100%;
		    border-style: none;
			border-spacing:0px ;
			border-collapse: collapse;
		}
		
		#left_bar{
		    vertical-align:top;
		    padding:5px;
		    width:180px;
		}
		
		#vertical_divider {
		    width:1px;
		    padding:0px;
		    background-color:#afafaf;
		    height:400px;
		}
		
		#content_container{
		    vertical-align:top;
		    padding:5px;
		    font-size:12px;
		}
		
		#search_term{
		    font-size:12px;
		}
		
		#external_links{
		    font-size:12px;
		}
		
		.divider{
		    height:0px;
		    border-top-style:dotted;
		    border-top-color:#afafaf;
		    border-collapse:collapse;
		    border-bottom-style:none;
		}
		
		
		.nodeDetailBox{
		    padding:2px;
		    background-color:#efefef;
		    margin-top:2px;
		    vertical-align:middle;
		}
		
		table.statementTableWrapper {
		    border-style: none;
			border-spacing:0px ;
			background-color:#fdfdfd;
			border-collapse: collapse;
			width:100%;
		}
		
		table.statementTableWrapper td {
		    border-style: none;
			border-spacing:0px ;
			background-color:#fdfdfd;
			border-collapse: collapse;
			width:100%;
			padding:10px;
		}
		
		
		.li_divider{
		    padding:0px;
		    margin:0px;
		    height:0px;
		    border-top-style:dotted;
		    border-top-color:#afafaf;
		    border-collapse:collapse;
		    border-bottom-style:none;
		}
		
		.fixed_width{
		    font-family: courier, fixed, monospace;
		    font-weight:normal;
		}
		
		table.annotationStatementTable{
		    font-size:12px;
		    width:100%;
		    border-style:none;
		    border-spacing:1px;
		    border-collapse:separate;
		    padding-top:5px;
		}
		
		table.annotationStatementTable th{
		    width:12px;
		    vertical-align:top;
		    padding-right:8px;
		    padding-top:0px;
		    
		}
		
		table.annotationStatementTable td{
		    padding:0px
		}
		
		.expandoImage{
		    vertical-align:bottom;
		    border:0px;
		    padding-top:0px;
		    padding-bottom:0px;
		    padding-left:0px;
		    padding-right:4px;
		}
		
		table.otherFormats {
		    width:100%;
		    font-size:11px;
		    border-style:none;
		    border-spacing:1px;
		    border-collapse:separate;
		    background-color:#acacac;
		    padding:0px;
		}
		
		table.otherFormats th{
		    padding-left:4px;
		    text-align:left;
		    background-color:#dfdfdf;
		}
		
		table.otherFormats td{
		    text-align:center;
		    padding:2px;
		    background-color:white;
		    width:18%;
		}
		
		.nodeDef{
		    color:#5c5c5c;
		    font-size:11px;
		}
		
		table.scoredNodeResults {
		    font-size:12px;
		    border-collapse:separate;
		    border-spacing:2px;
		    background-color:#ffffff;
		}
		
		
		table.scoredNodeResults th{
		    font-weight:normal;
		    text-align:left;
		    vertical-align:bottom;    
		    padding-left:2px;
		    padding-right:2px;
		    background-color:#fdfdfd;
		}
		
		table.scoredNodeResults td{
		    padding-left:2px;
		    background-color:#fdfdfd;
		}
				
			
	</style>
</#macro>

<#macro externalJS>
	<script type="text/javascript">
		function toggleTable(div_id,hostname){

			plus = new Image();
			plus.src = "http://" + hostname + "/OBDUI/images/plus-box.gif";
			minus = new Image();
			minus.src = "http://" + hostname + "/OBDUI/images/min-box.gif";
			
			var detailsDiv = document.getElementById(div_id);
			var toggleImage = document.getElementById((div_id+'_image'));
			
			if (detailsDiv && toggleImage){
							
				if (detailsDiv.style.display == "none"){
					detailsDiv.style.display='table-cell';
					toggleImage.src=minus.src;
				} else {
					detailsDiv.style.display="none";
					toggleImage.src=plus.src;
				}			
			}
		}
	</script>
</#macro>