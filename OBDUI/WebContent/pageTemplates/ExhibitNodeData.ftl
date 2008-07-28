{
	"items" : [
		<#if (statements?size >0)>
			<#list 0..statements?size-1 as statementCount>
				{
					id:"${statementCount}",
					label:"${statements[statementCount].subject.label?replace("\"","\\\"")} ${statements[statementCount].predicate.label?replace("\"","\\\"")} ${statements[statementCount].object.label?replace("\"","\\\"")}",
					type:"${statements[statementCount].aspect}",
					subject:"${statements[statementCount].subject.label?replace("\"","\\\"")}",
					<#if ((statements[statementCount].subject.source?exists)&&(statements[statementCount].subject.source != ""))>
						subjectSource: "${statements[statementCount].subject.source}",
					</#if>
					subjectHref: "${statements[statementCount].subject.href}",
					predicate:"${statements[statementCount].predicate.label?replace("\"","\\\"")}",
					predicateHref:"${statements[statementCount].predicate.href}",
					object:"${statements[statementCount].object.label?replace("\"","\\\"")}",
					<#if ((statements[statementCount].object.source?exists)&&(statements[statementCount].object.source != ""))>
					objectSource:"${statements[statementCount].object.source}",
					</#if>
					objectHref:"${statements[statementCount].object.href}"
				}<#if (statementCount != (statements?size-1))>,</#if>
			</#list>
		</#if>	
	]

}