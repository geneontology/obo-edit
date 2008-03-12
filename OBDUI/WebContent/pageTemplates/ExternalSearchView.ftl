<#include "PageMacros.ftl"> 

<@page title="Detail for: ${focusId}">
<h1>
search
</h1>


<@section title="Search URLs">
<dl>
<#list urlmap.keySet() as db>
<dt>${db}</dt>
<dd><a href='${urlmap.get(db)}'>${urlmap.get(db)}</a></dd>
</#list>
</dl>
</@section>

</@page>
