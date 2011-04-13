/**
   <h2>Annotation Quality Checks Rules<h2>
   
<p>
	This package provides classes which implements annotation rules to 
	find annotation violations. To implement a new check 
	a class to be inherited from the {@link org.geneontology.gold.rules.AbstractAnnotatioRule} class
	(See {@link org.geneontology.gold.rules.GoClassReferenceAnnotationRule, org.geneontology.gold.rules.CardinalityCheckRule} classes
	as an guide to implement a new rule).
</p>

<p>
	The instances (Annotation Rules) of the subclasses of the {@link org.geneontology.gold.rules.AbstractAnnotatioRule} class are
	created by reading the configuration file annotation_qc.xml. The annotation configuration file location is to be read via property
	the geneontology.gold.gaf.qcfile configured in the conf/gold.properties file. 
</p>

<p>
	The {@link org.geneontology.gold.rules.AnnotationRulesEngine} object read the annotation configuration file
	and create instances of the Annotation Rules. This object also run the rules to get the annotation violations.
</p>

<p>
	The class {@link org.geneontology.gold.rules.AnnotationRuglarExpressionFromXMLRule} general implementation for the regular expression rules
	(See annotation_qc.xml file for the regular expression rules).
</p>

@author Shahid Manzoor

 */
package org.geneontology.gold.rules;

