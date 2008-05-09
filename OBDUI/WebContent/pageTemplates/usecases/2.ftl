<#include "/PageMacros.ftl"> 

<h2>Use Case 2: Annotations for class of interest filtered by relation</h2>

<h3>See Also</h3>
<p>
This is a variant of <@usecase id="1"/>
<p>

<h3>Goal</h3>
To obtain annotations of relevance to a particular class of interest (COI), in which the relation of relevance can be specified
<ul>
<li>For a cell type, find out genes it <b>or its descendants</b> expresses
</li>
<li>
For a given cell type, find phenotypes that affect the morphology of <b>parts</b> of that 
cell
</li>
</ul>
<h3>Summary</h3>
<p>
As for <@usecase id="1"/>, with the extension such that the user can select a relation of interest. 
Annotations to classes that are linked to the COI by this relation are returned.
</p>

<h3>Pre-conditions</h3>
<p>
As for <@usecase id="1"/>
</p>

<h3>Triggers</h3>
One (but not all) of the following
<ul>
<li>
User requests annotation summary for COI <b>and</b> chooses a relation
</li>
<li>
User is already viewing annotations from <@usecase id="1"/>, and decided to filter search by choosing a relation
</li>
</ul>
</p>
<h3>Course of events</h3>

<h3>Post-conditions</h3>
<p>
As for <@usecase id="1"/>, with the additional criteria:
<ul>
 <li>
  Only annotations to classes linked to the COI via selected relation are shown
  </li>
 </ul>
 </p>

<h3>Guarantees</h3>
<p>
As for <@usecase id="1"/>
 </p>

<h3>Business rules</h3>
 <p>
  <ul>
   <li>OWL model-theoretic axioms apply</li>
   <li>weak association by default:
 	<p>
 	An annotation R(X,Y) is displayed if it can be proved that R'(Y,COI), where R' is the relation of interest, OR R' is a sub-relation of R
 	</p>
 	<p>
 	R' can be a reflexive relation
 	</p>
   </li>
  </ul>
  Example: <@obdurl path="node/CL:0000100/statements/annotations/develops_from"/>
 </p>