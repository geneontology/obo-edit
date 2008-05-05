<#include "/PageMacros.ftl"> 

<h2>Use Case 7: Find similar entities based on annotation profile</h2>
<h3>See Also</h3>
<p>
 <@usecase id="7"/>
</p>
<h3>Goal</h3>
<p>
To obtain similar entities to an entity of interest based on an annotation profile. For example,
a researcher may be viewing a information on a gene that is implicated in a disease of interested
(for example, a neurological disease). The gene may be associated with various phenotypes
observed from a variety of methods at a variety of scales - from the permeability of mitochondrial
cristae in a certain cell types to organismal/behavioural phenotypes. The goal is to
find similar entities with similar profiles - these entities may be genes, genotypes, trials,
individuals in trials, annotated images, publications.
</p>
<p>
 It is unlikely that profiles will match identically - the goal is to find a match
 similar to a subset of statistically significant ontological attributes, taking into account
 relations between these attributes. Think match.com or amazon book recommendation, with ontologies
 </p>
 <h3>Summary</h3>
<p>
The user arrives at an EOI (entity of interest) in BioPortal via search or browsing annotations; see
<@usecase id="5"/>
for example they may have arrived at a page showing the <@hrefSearch term="shh"/> (sonic hedgehpog) gene through annotations
to the ontology class for the disease/phenotype "holoprosencephaly". They see that shh has a long list
of annotations associated, directly or indirectly. Rather than explore each individually they
instead choose to find similar entities based on the whole profile.
</p>
<p>
When they select this option, they see a list of entities together with minimal metadata (type, label, ID)
plus match/similarity scores. These can be expanded to see the full list of annotations, possibly with
the similar annotations highlighted.
<p/>
<h3>Pre-conditions</h3>
<p>
As for <@usecase id="1"/>
</p>
<h3>Triggers</h3>
The following in sequence
<ul>
<li>
User selects EOI within BioPortal (perhaps via some shopping cart)
</li>
<li>
User asks for all similar entities
</li>
</ul>
</p>

<h3>Course of events</h3>
<ul>
<li>
User select EOI within BioPortal (perhaps via some shopping cart)
</li>
<li>
User asks for all similar entities
</li>
<li>
presentation layer makes query to OBD REST. URLs loos like:
   <ul>
   <li> <@obdurl path="nodes/OMIM:601653.0008/blast"/>an OMIM allele record for eya1</li>
   <li> <@obdurl path="nodes/ZFIN:ZDB-GENO-980202-1557/blast"/> similar to this eya1 genotype in zfin</li>
</li>
</ul>
</ul>
<h3>Post-conditions</h3>
<p>
Query results satisfy business rules, below
</p>

<h3>Business rules</h3>
 <p>
 
 </p>   
<p>
 Statistical formula to go here. 
 </p>   
<h3>Variants</h3>
 <p>
 Intelligent expansion of EOI: for example, genotype to gene, gene to orthology set
 </p>   

