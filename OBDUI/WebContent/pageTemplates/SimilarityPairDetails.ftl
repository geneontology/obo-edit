<#include "PageMacros.ftl"> 
<#include "NodeDetailsMacro.ftl"> 
<html>
	<head>
		<@stylize/>
    	<@genTitle pageName="Similarity Pair Details"/>
    	
    </head>	
	<body>
		<@header/>	
		<table class="threecol">
			<tr>
				<td id="left_bar">
					<@searchForm/>
					<hr class="divider"/>
					Node Metadata goes here.
				</td>
				<td id="vertical_divider">
				</td>
				<td id="content_container">
					<h2>${node1.label} <span style="font-weight:normal;">vs</span> ${node2.label}</h2><br/>
					<table class="intersectionPairTable">
						<tr>
							<th class="set1"><@nodeHashHref nodeHash=node1/> Statements</th>
							<th class="set2"><@nodeHashHref nodeHash=node2/> statements</th>
						</tr>
						<tr>
							<td class="common">
								<ul>
									<#list intersectionNodes as node>
										<li><@nodeHashHref nodeHash=node/></li>
									</#list>
								</ul>
							</td>
							<td class="common">
								<ul>
									<#list intersectionNodes as node>
										<li><@nodeHashHref nodeHash=node/></li>
									</#list>
								</ul>
							</td>
						</tr>
						<tr>
							<td class="set1unique">
								<ul>
								<#list set1unique as node>
									<li><@nodeHashHref nodeHash=node/>
								</#list>
								</ul>	
							</td>
							<td class="set2unique">
								<ul>
								<#list set2unique as node>
									<li><@nodeHashHref nodeHash=node/></li>
								</#list>
								</ul>
							</td>
							
						</tr>
					</table>
					
				</td>
			</tr>
		</table>
		<@footer/>
	</body>
</html>
