<#include "PageMacros.ftl">
<#include "NodeDetailsMacro.ftl"> 
<html>
	<head>

		<link rel="stylesheet" type="text/css" href="/OBDUI/css/jquery.autocomplete.css" />

		<style type="text/css">
			input.predicateElement {
				color:#999999;
			}
			
			span.logicSwitch{
				font-family:courier,courier new;
			}
			
			
		</style>

		<script type="text/javascript" src="/OBDUI/js/jquery/jquery-1.2.6.min.js"></script>
		<script type="text/javascript" src="/OBDUI/js/jquery/jquery.autocomplete.js"></script>
		<script type="text/javascript" src="/OBDUI/js/jquery/jquery.bigframe.min.js"></script>
		<script type="text/javascript" src="/OBDUI/js/jquery/jquery.dimensions.js"></script>
		<script type="text/javascript" src="/OBDUI/js/jquery/thickbox-compressed.js"></script>
				
		<script type="text/javascript">
			$(document).ready(
				function(){
					appendAddQueryElement($("#query"));
					//$("#query").addClass("subQuery");
					$("#submit").click(
						function(){
							window.location=('/OBDUI/obdPhenotypeAll2008/html/query/' + encodeURIComponent(deconstruct("#query")));
						}
					);
				}
				
			);
			
			function deconstruct(element){
			
				var open = '[';
				var close = ']';
				var queryString = '';
				
				if ($(element).hasClass("booleanQuery")){
					queryString += (getLogic(element) + open);
					var children = $(element).children(".queryElement");
					for (var i=0;i<children.size();i++){
						queryString += '+'  + deconstruct(children[i]);
					}	
					
					queryString += close;
				} else if ($(element).hasClass("linkQuery")){
					queryString += ('link' + open);
					if ($(element).children("input.predicateValue").val() != 'undef'){
						queryString += ('?predicate="' + $(element).children("input.predicateValue").val() + '"');
					}
					if ($(element).children("input.objectValue").val() == 'subQuery'){
						queryString += ('?object=' + deconstruct($(element).children(".subQuery")[0]));
					} else if ($(element).children("input.objectValue").val() != 'undef'){
						queryString += ('?object="' + $(element).children("input.objectValue").val() + '"');
					}
					queryString += close;
				} else if ($(element).hasClass("subQuery")) {
					queryString += deconstruct($(element).children(".queryElement")[0]);
				} else {
					alert("What kind of query? " + $(element).val());
				}
				
				
				return queryString;
			}
			
			function linkQueryDeconstruct(element){
				var queryString = 'link';
				
			}
						
			function genInputElement(name,url){
			
				var oie = document.createElement("input");
				$(oie).addClass((name + "Element"));
				$(oie).val(name);
				$(oie).css("color","#999999");
				$(oie).attr("type","text");
				
				var oiev = document.createElement("input");
				$(oiev).attr("type","hidden");
				$(oiev).addClass((name + "Value"));
				$(oiev).val("undef");
				
				// Autocomplete ajax call invoke
				$(oie).autocomplete(url,{ 
					minChars: 3,
					delay: 500, 
					width:400, 
					max:100,
					formatItem:	function(row,i,max){
									var parts = String(row).split(",");
									return parts[0] + ": " + parts[1];
								},
					formatResult: function(row) {
									var parts = String(row).split(",");
									return parts[1];
								  }
					}
				);
				
				// Autocomplete result handler sets the hidden input value
				$(oie).result(
					function findValueCallback(event, data, formatted) {
						var parts = String(data).split(",");
						$(oiev).val(parts[0]);
					}
				);
				
				// Blur handler sets the hidden input value to the text field if nothing was found by the autocomplete entry
				$(oie).blur(
					function(){
						if ($(oie).val() == ''){
							$(oie).val(name);
							$(oie).css("color","#999999");
							$(oiev).val('undef');
						} else {
							if ($(oiev).val() == 'undef'){
								$(oiev).val($(oie).val());
							}
						}
					}
				)
					
				$(oie).focus(
					function(){
						if ($(oie).val() == name){
							$(oie).val('');
							$(oie).css("color","#000000");
						}
					}
				);
				
				return new Array(oie, oiev);
			}
			
			function getLogicElement(element){
				var ile = document.createElement("span");
				$(ile).addClass("logicSwitch");
				$(ile).html($(element).children("input.logic")[0].value);
				$(ile).click(
					function(){
						var otherLogic;
						if ($(element).children("input.logic")[0].value == 'and'){
							var otherLogic = 'or';
						} else {
							var otherLogic = 'and';
						}
						$(element).children("input.logic")[0].setAttribute("value",otherLogic);
						$(element).children("span.logicSwitch").html(otherLogic);
					}
				);
				return ile;
			}
			
			function setLogic(element,logic){
				$(element).children("input.logic").val(logic);
			}
			
			function getLogic(element){
				return $(element).children("input.logic").val();
			}
			
			function genBasicQueryTermLink(parentElement){
				var bae = document.createElement("a");
				$(bae).html("+");
				$(bae).click( 
					function(){
						if ($(parentElement).children(".queryElement").size()==1){
							var hie = document.createElement("input");
							hie.setAttribute("type","hidden");
							$(hie).addClass("logic");
							$(parentElement).append(hie);
							setLogic(parentElement,"and");
							
						}
						addBasicQueryTerm($(parentElement));
					} 
				);
				return bae;
			}

			function addSubQuery(element){
				$(element).children("li.addElement").remove();
				if ($(element).children("li.queryElement").size()>0){
					$(element).append(getLogicElement(element));
				}
				$(element).append($(genSubQuery()).addClass("queryElement"));
				appendAddQueryElement(element);
				
			}
			

			function genBooleanQueryTermLink(parentElement){
				var bqte = document.createElement("a");
				$(bqte).html("&loz;");
				$(bqte).click(
					function(){
						if ($(parentElement).children(".queryElement").size()==1){
							var hie = document.createElement("input");
							hie.setAttribute("type","hidden");
							$(hie).addClass("logic");
							$(parentElement).append(hie);
							setLogic(parentElement,"and");
						}
						addSubQuery($(parentElement));
					}
				);
				return bqte;
			}
			
			
			function genSubQuery(){
				var ule = document.createElement("ul");
				$(ule).addClass("subQuery");
				addBasicQueryTerm(ule);
				return ule;
			}
			
			function removeSubQuery(element){
				$(element).children("ul.subQuery").remove();
				$(element).children("a.logicSwitch").remove();
				$(element).children("input.objectValue").remove();
				
			}
			
			function getLogicSwitch(element,position){
				var lse = document.createElement("a");
				$(lse).addClass("logicSwitch");
				if (position == 'off'){
					$(lse).html("&rfloor;");
					$(lse).click(
						function(){
							$(element).children("input.objectElement").remove();
							$(element).children("a.logicSwitch").remove();
							$(element).children("input.objectValue").val('subQuery');
							$(element).append(getLogicSwitch(element,'on'));
							$(element).append(genSubQuery());
						}
					);
				} else {
					$(lse).html("&rceil;");
					$(lse).click(
						function(){
							removeSubQuery(element);
							$(element).append(genInputElement("object","/OBDUI/obdPhenotypeAll2008/autocomplete/search/nodes/"));
							$(element).append(getLogicSwitch(element,'off'));
						}
					);
				}
				return lse;
				
			}
			
			function addBasicQueryTerm(element){
				var liElement = document.createElement("li");
				
				$(liElement).addClass("queryElement");
				$(liElement).addClass("linkQuery");
				
				$(liElement).append(genInputElement("predicate","/OBDUI/obdPhenotypeAll2008/autocomplete/search/relations/"));
				$(liElement).append(genInputElement("object","/OBDUI/obdPhenotypeAll2008/autocomplete/search/nodes/"));
				$(liElement).append(getLogicSwitch(liElement,'off'));
				if ($(element).children("li.queryElement").size()>0){
					$(element).prepend(getLogicElement(element));
				}
				$(element).prepend(liElement);
				
				appendAddQueryElement(element);
				if ($(element).children(".queryElement").size()>1){
					$(element).addClass("booleanQuery");
				}
			}
			
			function appendAddQueryElement(element){
				if ($(element).children("li.addElement").size()==0){
					var lie = document.createElement("li");
					$(lie).addClass("addElement");
					$(element).append(lie);
				} 

				if ($(element).children("li.queryElement").size() == 0){
					$(element).children("li.addElement").append(genBasicQueryTermLink(element));
				} else {
					$(element)
					.children("li.addElement").html("").append(genBasicQueryTermLink(element)).append(" / ").append(genBooleanQueryTermLink(element));
				}
			}
			
		</script>


		<@genTitle pageName="Query Results"/>
		<@stylize/>
	</head>
	<body>
        <@header/>
		<table class="threecol">
			<tr>
				<td id="left_bar">
					<@searchForm/>
					<hr class="divider"/>
				</td>
				<td id="vertical_divider">
				</td>
				<td id="content_container">
					<h2><span style="font-weight:normal;">OBD Query Builder</span> </h2><br/>
					<br/>
					<ul id="query" class="subQuery">
					</ul>
					<input type="submit" id="submit" value="Submit Query"/>
				</td>
			</tr>
		</table>
		<@footer/>
	</body>
</html>
