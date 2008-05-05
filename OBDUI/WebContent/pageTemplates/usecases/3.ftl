<#include "/PageMacros.ftl"> 

<h2>Use Case 3: Annotations for multiple classes of interest</h2>

<h3>See Also</h3>
<p>
This is a variant of <@usecase id="1"/>
<p>
<h3>Goal</h3>
To obtain annotations of relevance to a particular <b>set of</b> classes of interest (COI).
 This may be for the <b>intersection</b> (see below) or </b>union</b> of annotations that conform
Example:
<ul>
<li>For 2 cell types, find out genes expressed in either
</li>
<li>For a cell type and a gross anatomical entity, find genes expressed in both
</li>
<li>For quality class and another class, find annotations to phenotypes that are the class intersection
</li>
</ul>

<h3>Summary</h3>
<p>
An extension of <@usecase id="1"/>. The difference is <b>multiple classes</b> can be selected. These can be used in an all of vs one of sense
</p>

<h3>Pre-conditions</h3>
<p>
As for <@usecase id="1"/>
</p>

<h3>Triggers</h3>
The following in sequence
<ul>
<li>
User selects multiple COIs within BioPortal (perhaps via some shopping cart)
</li>
<li>
User asks for all annotations to ONE OF vs ALL OF these classes.
</li>
</ul>
</p>

<h3>Course of events</h3>
<ul>
<li>
User selects multiple COIs within BioPortal (perhaps via some shopping cart)
</li>
<li>
User asks for all annotations to ONE OF vs ALL OF these classes.
</li>
<li>
presentation layer makes query to OBD REST. URLs loos like:
   <ul>
   <li> <@obdurl path="nodes/MP:0001306+FMA:58241"/> (small lens & Lens)</li>
   <li> <@obdurl path="nodes/MP:0001306+FMA:58241[ONEOF]"/> (same as above)</li>
   <li> <@obdurl path="nodes/MP:0001306+FMA:58241[ALLOF]"/> </li>
</li>
</ul>

</ul>
<h3>Post-conditions</h3>
<p>
Query results satisfy business rules, below
</p>

<h3>Business rules</h3>
 <p>
   <ul>
   <li>
   For ONEOF: the union of annotations to each of the specified classes (as in <@usecase id="1"/> is included)
   </li>
   <li>
   For ALLOF: an annotation R(X,Y) is only included if there exists annotations between X and all COIs
   </li>
   </ul>
 </p>