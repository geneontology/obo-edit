<#include "/PageMacros.ftl">

<h2>Use Case 1: Annotations for class of interest</h2>

<h3>To obtain annotations of relevance to a particular class of interest (COI). For example:</h3>
<ul>
<li>For a cell type, find out genes it expresses (and the processes they execute therein), phenotypes involving morphology of that cell type
</li>
<li>
For a disease, genes that are implicated or trials involving that disease
</li>
</ul>


<h3>Summary</h3>
<p>
A BioPortal user arrives at a class of interest (COI) in the course of using BP. They may be on a detail page for that class; or they may be on a page summarising search results,
or some other ontology filter. They choose to see all annotations for this COI - this includes annotations both <b>directly</b> to the COI, and annotations in which a relation between
the annotated entity and COI can be <b>inferred</b>.
</p>
<p>
 EXAMPLE: A researcher is interested in motor neurons (neurons that project axons outsides the central nervous system to activate muscle cells). There may be many researchers with mant reasons for this interest:
 <ul>
 <li>they may have observed a phenotype in which motor neurons are affected, and want to find similar phenotypes</li>
 <li>they may be interested in diseases in which motor neurons play a role (eg Parkinsons), and want to find animal models of this disease, or clinical investigations involving this cell type</li>
 <li>they may be basic researchers, interested in development of the nervous system</li>
 </ul>
 
 </p>
 <p>
 BioPortal may provide access to classes representating of this cell type in different ontologies. One such class is <i><@hrefNode id="CL:0000100"/> (motor neuron)</i> from the OBO Cell ontology.
 BioPortal would give metadata about this class, together with logical axioms (eg motor neuron is_a neuron). Some of these axioms may span ontologies (motor neuron connected to muscle cell; neuron has_part axon).
 The page describing this class would have a link to a tab to get annotations to this class.
</p>

<h3>Pre-conditions</h3>
<ul>
 <li>
  User is viewing BP page showing COI
 </li>
 <li>
  Annotations have been loaded into OBD
 </li>
 <li>
  Relevant ontology axioms are loaded in into OBD
 </li>
 <li>
  Deductive closure has been completed to an particular level of completeness
 </li>
 </ul>

<h3>Triggers</h3>
<p>
User requests annotation summary for COI
</p>

<h3>Course of events</h3>
<ul>
<li>
User selects COI (eg, the class for <i>motor neuron</i>) via presentation layer
</li>
<li>
Presentation layer requests information from OBD REST Service. Uses URL such as <@obdurl path="node/CL:0000100/statements/annotations"/>
</li>
<li>
Service responds with message in some pre-defined concrete serialization (eg obd-json, owl1.1xml)
</li>
<li>
Presentation layer may request further information on the nodes returned
</li>
<li>
Presentation layer transforms message into displayable form, and presents to user
</li>
</ul>

<h3>Post-conditions</h3>
<p>
<ul>
 <li>
  All annotations fulfilling query criteria are displayed to the user. The match criteria for a simple query on a COI are as follows:
 <ul>
 <li>
  The annotation posits a link of relation R between X and some Y, 
  where X denotes an annotated entity, and Y is typically a class
  </li>
  <li>
  It can be proved that X stands in relation R' to COI (for example, if COI is subsumed by Y)
  </li>
 </ul>
 </li>
 <li>
  Annotations are summarised such that all annotations asserted to a particular class (Y) are grouped together. Class details are shown in a succinct summary (e.g. ID, label, definition)  
 </li>
 <li>
  Beneath each class, each annotation to that class is shown as a separate row
 </li>
 <li>
  The row consists of succinct details concerning the annotated entity (X), and metadata for the annotation
 </li>
<li>
  The succint information for X can include the number of annotations X has to other classes
 </li>
 <li>
  The metadata for an annotation is not constrained, and consists of a number of statement-level statements, including, but not limited to provenance, evidence, source
 </li>
 <li>
  All entities shown are hyperlinked to more detailed information. For example, the annotated entity would link
  to an entity detail page as described in <@usecase id="5"/>
 </li>
</ul>
</p>

<h3>Guarantees</h3>
 <ul>
  <li>
   The list of annotations excludes annotations to classes that are unrelated to the class of interest
  </li>
 </ul>

<h3>Business rules</h3>
 <p>
  <ul>
   <li>OWL model-theoretic axioms apply</li>
   <li>weak association by default:
 	<p>
 	An annotation R(X,Y) is displayed if it can be proved that R'(Y,COI), where R' can be any relation
 	</p>
 	<p>
 	Here R and R' denote either ABox facts or TBox axioms (subclass or existential restriction)
 	</p>
   </li>
  </ul>
 </p>
 
 <p>
 Examples for COI <@hrefNode id="MP:0000937"/> (abnormal motor neuron morphology)
 <ul>
  <li>
   The direct annotation of genotype <@hrefNode id="Shh%3Ctm1Chg%3E%2FShh%3Ctm1Chg%3E" label="Shh<sub>tm1Chg</sub>"/> (a mouse <@hrefNode id="SO:0001027" label="genotype"/> in which the gene <@hrefNode id="" label="Shh"/> is mutated)
   to mammalian phenotype class <@hrefNode id="MP:0000937"/> (<i>abnormal motor neuron morphology</i>) is shown.
   No logical deduction is required here: the annotation is directly to the COI
  </li>
 <li>
   The  annotation of genotype <@hrefNode id="Cntf%3Ctm1Mpin%3E%2FCntf%3Ctm1Mpin%3E" label="Cntf<sub>tm1Mpin</sub>"/>
    (a mouse <@hrefNode id="SO:0001027" label="genotype"/>)
   to mammalian phenotype class <@hrefNode id="MP:0000938"/> (<i>motor neuron degeneration</i>) is shown.
   Simple asserted subsumption (is_a) links this class to the COI
  </li>
 </ul>
 </p>
 
   <p>
 Examples for COI <@hrefNode id="CL:0000100"/> (motor neuron)
 <ul>
  <li>
   The annotation of genotype <@hrefNode id="Shh%3Ctm1Chg%3E%2FShh%3Ctm1Chg%3E" label="Shh<sub>tm1Chg</sub>"/> (a mouse <@hrefNode id="SO:0001027" label="genotype"/> in which the gene <@hrefNode id="" label="Shh"/> is mutated)
   to mammalian phenotype class <@hrefNode id="MP:0000937"/> (<i>abnormal motor neuron morphology</i>) is shown.
   This is because this MP class has been given a logical definition in MP-XP involving CL:0000100. See <@obdurl path="node/MP:0000937/description/"/>, linking to the COI via the inheres_in relation
  </li>
  <li>
  The annotation of zebrafish genotype <@hrefNode id="ZFIN:ZDB-GENO-071004-3" label=""/> to the <i>class expression</i>
  <@hrefNode id="PATO%3A0000419%5Eduring%28ZFIN%3AZDB-STAGE-010723-16%29%5Einheres_in%28CL%3A0000533%5Epart_of%28ZFA%3A0000075%29%29"
    label="PATO:decreased number THAT inheres_in CL:primary motor neuron THAT part_of ZFA:spinal cord"/> is shown because
    this class expression can be shown to be linked to motor neuron via inheres_in. You can see the class expression description here: 
    <@obdurl path="node/PATO:0000419%5Eduring(ZFIN:ZDB-STAGE-010723-16)%5Einheres_in(CL:0000533%5Epart_of(ZFA:0000075))/description/" format="owl"/>,
    and the full pre-reasoned subsumption path here: <@obdurl path="node/PATO:0000419%5Eduring(ZFIN:ZDB-STAGE-010723-16)%5Einheres_in(CL:0000533%5Epart_of(ZFA:0000075))/statements/about/"/>
  </li>
 </ul>
 </p>
   
