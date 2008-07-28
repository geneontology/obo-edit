<#include "PageMacros.ftl"> 
<#include "NodeDetailsMacro.ftl"> 
<html>
	<head>
		<@stylize/>
    	<@genTitle pageName="Node Details"/>
		
		   <script src="http://static.simile.mit.edu/exhibit/api-2.0/exhibit-api.js"
		           type="text/javascript"></script>
		           
		   <script src="http://static.simile.mit.edu/exhibit/extensions-2.0/time/time-extension.js"
		           type="text/javascript"></script>
    	
		   <link href="/OBDUI/${dataSource}/exhibitData/node/${encodedId}" type="application/json" rel="exhibit/data"/>
		
		<style type="text/css">
			.exhibit-viewPanel-viewContainer {
				font-size:12px;
			}
			.entry{
				padding:2px;
			}
			.exhibit-viewPanel-viewContainer h1 {
				background-color:#E8E8E8;
				padding:4px;
				padding-left:8px;
				font-size:14px;
				font-weight:normal;
			}
			.exhibit-viewPanel-viewContainer h2 {
				background-color:#EFEFEF;
				padding:4px;
				padding-left:8px;
				font-size:13x;
				font-weight:normal;
			}
       	</style>
    </head>	
	<body>
		<@header/>	
		<table class="threecol">
			<tr>
				<td id="left_bar">
					<@searchForm/>
					<#if node?exists>
						<hr class="divider"/>
						<@phat id=encodedId/>
						<hr class="divider"/>
						<@altViews nodeViews=nodeViews/>
						Results Filter:
						<div style="font-size:11px;" ex:role="facet" ex:expression=".type" ex:facetLabel="Statement Type"></div>
						<div style="font-size:11px;" ex:role="facet" ex:expression=".predicate" ex:facetLabel="Relationship Type"></div>
						<div style="font-size:11px;" ex:role="facet" ex:expression=".subjectSource" ex:facetLabel="Subject Source"></div>
						<div style="font-size:11px;" ex:role="facet" ex:expression=".objectSource" ex:facetLabel="Target Source"></div>
					</#if>
				</td>
				<td id="vertical_divider">
				</td>
				<td id="content_container">
					<#if node?exists>
						<h2>${node.id}
							<#if node.isComposed?exists>:<@labelDecompose node=node.composedNode/></h2>
							<#else>
								<#if node.label?has_content>: ${node.label}</#if></h2>
							</#if>
						<br/>
						
						<table width="100%">
							<tr valign="top">
								<td ex:role="viewPanel">
									<span class="entry" ex:role="lens">
									<a ex:href-content=".subjectHref"><span ex:content=".subject"></span></a> <a ex:href-content=".predicateHref"><span ex:content=".predicate"></span></a> <a ex:href-content=".objectHref"><span ex:content=".object"></span></a>
									</span>
									
									<!--
										<div ex:role="exhibit-view"
											ex:viewClass="Exhibit.TabularView"
											ex:columns=".label, .imageURL, .discipline, .nobel-year, .relationship-detail"
											ex:columnLabels="name, photo, discipline, year, relationship with MIT"
											ex:columnFormats="list, image, list, list, list"
											ex:sortColumn="3"
											ex:sortAscending="false">
										</div>
										<div ex:role="view"
											ex:viewClass="Timeline"
											ex:start=".nobel-year"
											ex:colorKey=".discipline">
										</div>
										<div ex:role="view"
											ex:orders=".discipline, .nobel-year"
											ex:possibleOrders=".label, .last-name, .discipline, .relationship, .shared, .deceased, .nobel-year">
										</div>
									-->
									<div ex:role="view"
											ex:orders=".type"
											ex:possibleOrders=".type,.predicate,.subject,.object">
									</div>
								</td>

					       </tr>
					   </table>
						
					<#else>
						<h2>Node ${id} Not Found</h2>
					</#if>
				</td>
			</tr>
		</table>
		<@footer/>
	</body>
</html>
