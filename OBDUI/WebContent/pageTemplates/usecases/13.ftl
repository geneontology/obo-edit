<#include "/PageMacros.ftl"> 

<h2>Use Case 13: Quantification over annotation mapping</h2>

<h3>See Also</h3>
<p>
This is essentially an aggregate query variation of <@usecase id="12"/>
</p>
<h3>Goal</h3>
<p>
Given a large collection of annotations to highly specific/granular ontology classes, the user
would like to see a broad summary of all annotations categorized according to a particular subset/view/segment
of some ontology
</p>
<h3>Summary</h3>
<p>
	Given a set of annotations and a subset/view over an ontology(s), map all
	annotations to the subset and then count the number of DISTINCT annotated
	entities related to the mapped class. The deductive closure is used here.
	For example, if the shard contains the following annotations:
	
	<table border="1">
	  <tr>
	  <td>genotype1</td>   <td>size of Eye</td>
	  </tr>
	  <tr>
	  <td>genotype2</td>   <td>thickness of Wall of eyeball</td>
	  </tr>
	  <tr>
	  <td>genotype2</td>   <td>color of Iris</td>
	  </tr>
	  <tr>
	  <td>genotype3</td>   <td>Thumb</td>
	  </tr>
	</table>
	
	Then the count for "Eye" would be 2, because both genotypes are related to
	"Eye" via some chain of relations (see <a href="#Variants">below</a> for a discussion of this issue)
</p>
<p>
  Given a subset containing only <b>finger</b> and <b>eye</b> from the full FMA, quantification of 
  annotated entities (above) would yield:
	<table border="1">
	  <tr>
	  <td>Eye</td>   <td>2</td>
	  </tr>
	  <tr>
	  <td>Finger</td>   <td>1</td>
	  </tr>
	</table>



</p>
<p>
The user may obtain results as raw data, tabular summary, or via some kind of graphic. Choice of graphics
include pie charts and bar charts. One thing to be wary of is that pie charts sometimes carry with them an implicit
assumption of mutual exclusivity. It may only be appropriate to display pies when the ontology explicitly declares
mutual exclusivity of its classes (e.g. via disjointWith axioms). 
</p>
<p>
<a href="/OBDUI/images/map2slim-screenshot.png"><img src="/OBDUI/images/map2slim-screenshot-thumb.png"></a>
</p>

<h3>Pre-conditions</h3>
<ul>
 <li>
 A particular segment or subset has been pre-selected and named.
 </li>
<li>
 The deductive closure of classes (named and class expressions) used in annotation is available,
 either via pre-computation or dynamic calculation
 </li>
</ul>
<h3>Triggers</h3>
<p>
User elects to see annotations quantified by a subset. This may happen after ontology browsing;
the user may be notified that a COI belongs to a particular subset. They may then see metadata on
this subset, together with annotation summaries
</p>

<h3>Course of events</h3>
<h3>Post-conditions</h3>
<p>
No change in state happens
</p>
<h3>Business rules</h3>
<h3>Variants</h3>
<p>
 Some kind of <@usecase id="4" label="Annotation filtering"/> can be used. For example, it may be desirable
 to get a subset summary broken down by annotation provenance and/or source
</p>
<p>
 The user may be given a choice of which relation to apply in mapping to the subset. For example,
 it may be desirable to propagate annotations up part_of links, but not develops_from. The default is
 to propagate over all relations, which may result in false positives
</p>
<p>
 Dynamic subsets/segments can be used. The default is to use <i>named</i> (ie pre-defined) subsets only.
 Some users may like to define a subset dynamically: for example, all classes n steps below "Organ", or
 all classes with an information content above <i>i</i>.
</p>
