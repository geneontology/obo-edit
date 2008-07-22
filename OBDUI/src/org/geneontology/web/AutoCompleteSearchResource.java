
package org.geneontology.web;


import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import org.obd.model.Node;
import org.obd.query.ComparisonQueryTerm.Operator;
import org.obd.query.LabelQueryTerm.AliasType;
import org.obd.ws.coreResource.NodeResource;
import org.obd.ws.coreResource.sorter.AutocompleteNodeSorter;
import org.restlet.Context;
import org.restlet.data.MediaType;
import org.restlet.data.Reference;
import org.restlet.data.Request;
import org.restlet.data.Response;
import org.restlet.resource.Representation;
import org.restlet.resource.StringRepresentation;
import org.restlet.resource.Variant;



/**
 * Resource for a node
 */
public class AutoCompleteSearchResource extends NodeResource {

	protected String searchTerm;
	protected String target;
	protected List<Node> nodes;
	

	/**
     * Constructor.
     * 
     * @param context
     *                The parent context.
     * @param request
     *                The request to handle.
     * @param response
     *                The response to return.
	 * @throws Exception 
     */
    public AutoCompleteSearchResource(Context context, Request request, Response response) throws Exception {
        super(context, request, response);
        
        this.searchTerm = (String) request.getAttributes().get("term");
        this.searchTerm = Reference.decode(searchTerm);
        
        this.target = (String) request.getAttributes().get("target");
        if (!this.target.equals("relations")){
        	this.target = "nodes";
        }
        
        getVariants().add(new Variant(MediaType.TEXT_PLAIN));
                
    }

 
    /**
     * Finds the associated node.
     * 
     * @return The node found or null.
     * @throws Exception 
     */
    public Collection<Node> findNodes() throws Exception {
        this.nodes = new ArrayList<Node>();
        this.nodes.addAll(getShard(this.dataSource).getNodesBySearch(searchTerm,Operator.CONTAINS,null,AliasType.PRIMARY_NAME));
        AutocompleteNodeSorter sorter = new AutocompleteNodeSorter();
        sorter.setPattern(this.searchTerm);
        Collections.sort(nodes,sorter);
        return nodes;
    }

 
    @Override
    public Representation getRepresentation(Variant variant) {
    	
    	StringBuilder sb = new StringBuilder();
    	
    	if (this.target.equals("relations")){
    		sb = this.getRelations(this.searchTerm);
    	} else {
	    	try {
				this.findNodes();
			} catch (Exception e1) {
				System.err.println("Error finding nodes: " + e1.getMessage());
				e1.printStackTrace();
			}
			
	    	
	
	    	for (Node n : this.nodes){
	    		sb.append(n.getId() + "|" + n.getLabel() + "\n");
	    	}
    	} 

    	return new StringRepresentation(sb.toString());
    	
    }

    private StringBuilder getRelations(String queryTerm){
    	// This is hardcoded until we extend the query api to target just relation nodes.
    	// SB string generated from command: psql -U postgres -h spade -c "select '\"' || uid || '|' || label || '\\\n\" + ' from node where metatype='R' and label is not null;" obd_phenotype_200805
    	StringBuilder matches = new StringBuilder();
    	StringBuilder allRelations = new StringBuilder("has_rank|has taxonomic rank\n" + 
    			 "overlaps|overlaps\n" + 
    			 "homologous_to|homologous_to\n" + 
    			 "oboInOwl:isCyclic|is_cyclic\n" + 
    			 "oboInOwl:savedBy|saved_by\n" + 
    			 "oboInOwl:replacedBy|replaced_by\n" + 
    			 "oboInOwl:inSubset|in_subset\n" + 
    			 "oboInOwl:hasVersion|has_version\n" + 
    			 "oboInOwl:hasURI|has_URI\n" + 
    			 "oboInOwl:hasSynonymType|has_synonym_type\n" + 
    			 "oboInOwl:hasSubset|has_subset\n" + 
    			 "oboInOwl:hasRelatedSynonym|has_related_synonym\n" + 
    			 "oboInOwl:hasSynonym|has_synonym\n" + 
    			 "oboInOwl:hasOBONamespace|has_obo_namespace\n" + 
    			 "oboInOwl:hasNarrowSynonym|has_narrow_synonym\n" + 
    			 "oboInOwl:hasExactSynonym|has_exact_synonym\n" + 
    			 "oboInOwl:hasDefinition|has_definition\n" + 
    			 "oboInOwl:hasDefaultNamespace|has_default_namespace\n" + 
    			 "oboInOwl:hasDbXref|has_dbxref\n" + 
    			 "oboInOwl:hasDate|has_date\n" + 
    			 "oboInOwl:hasBroadSynonym|has_broad_synonym\n" + 
    			 "oboInOwl:hasAlternativeId|has_alternative_id\n" + 
    			 "oboInOwl:consider|consider\n" + 
    			 "oboInOwl:ObsoleteProperty|obsolete_property\n" + 
    			 "start|start\n" + 
    			 "preceded_by|preceded_by\n" + 
    			 "end|end\n" + 
    			 "is_a|is a\n" + 
    			 "variant_of|variant_of\n" + 
    			 "similar_to|similar_to\n" + 
    			 "sequence_of|sequence_of\n" + 
    			 "regulated_by|regulated_by\n" + 
    			 "position_of|position_of\n" + 
    			 "partial_evidence_for_feature|partial_evidence_for_feature\n" + 
    			 "paralogous_to|paralogous_to\n" + 
    			 "orthologous_to|orthologous_to\n" + 
    			 "non_functional_homolog_of|non_functional_homolog_of\n" + 
    			 "has_quality|has_quality\n" + 
    			 "has_origin|has_origin\n" + 
    			 "has_genome_location|has_genome_location\n" + 
    			 "genome_of|genome_of\n" + 
    			 "associated_with|associated_with\n" + 
    			 "adjacent_to|adjacent_to\n" + 
    			 "OBO_REL:instance_of|instance_of\n" + 
    			 "is_tautomer_of|is tautomer of\n" + 
    			 "is_conjugate_acid_of|is conjugate acid of\n" + 
    			 "is_conjugate_base_of|is conjugate base of\n" + 
    			 "is_part_of|is part of\n" + 
    			 "OBO_REL:relative_to|OBO_REL:relative_to\n" + 
    			 "has_output|has_output\n" + 
    			 "after|after\n" + 
    			 "arises_from|arises_from\n" + 
    			 "has_qualifier|qualifier\n" + 
    			 "regulates|regulates\n" + 
    			 "positively_regulates|positively_regulates\n" + 
    			 "negatively_regulates|negatively_regulates\n" + 
    			 "is_unit_of|is_unit_of\n" + 
    			 "is_measurement_of|is_measurement_of\n" + 
    			 "is_magnitude_of|is_magnitude_of\n" + 
    			 "OBO_REL:interacts_with|interacts_with\n" + 
    			 "OBO_REL:overlaps|overlaps\n" + 
    			 "OBO_REL:has_qualifier|has_qualifier\n" + 
    			 "OBO_REL:has_duration|has_duration\n" + 
    			 "OBO_REL:has_boundary|has_boundary\n" + 
    			 "OBO_REL:has_aggregate_part|has_aggregate_part\n" + 
    			 "OBO_REL:subslice_of|subslice_of\n" + 
    			 "OBO_REL:stimulates|stimulates\n" + 
    			 "OBO_REL:costimulates_activation_of|costimulates_activation_of\n" + 
    			 "OBO_REL:develops_into|develops_into\n" + 
    			 "OBO_REL:inactivates|inactivates\n" + 
    			 "OBO_REL:activates|activates\n" + 
    			 "OBO_REL:results_in_connection_of|results_in_connection_of\n" + 
    			 "OBO_REL:results_in_adhesion_to|results_in_adhesion_to\n" + 
    			 "OBO_REL:results_in_attachment_of|results_in_attachment_of\n" + 
    			 "OBO_REL:results_in_death_of|results_in_death_of\n" + 
    			 "OBO_REL:results_in_transport_of|results_in_transport_of\n" + 
    			 "OBO_REL:results_in_release_from|results_in_release_from\n" + 
    			 "OBO_REL:results_in_release_of|results_in_release_of\n" + 
    			 "OBO_REL:results_in_closure_of|results_in_closure_of\n" + 
    			 "OBO_REL:unfolds_around|unfolds_around\n" + 
    			 "OBO_REL:unfolds_in|unfolds_in\n" + 
    			 "OBO_REL:results_in_transport_through|results_in_transport_through\n" + 
    			 "OBO_REL:results_in_transport_to|results_in_transport_to\n" + 
    			 "OBO_REL:results_in_transport_from|results_in_transport_from\n" + 
    			 "OBO_REL:results_in_structural_change_to|results_in_structural_change_to\n" + 
    			 "OBO_REL:results_in_positive_selection_of|results_in_positive_selection_of\n" + 
    			 "OBO_REL:results_in_negative_selection_of|results_in_negative_selection_of\n" + 
    			 "OBO_REL:results_in_selection_of|results_in_selection_of\n" + 
    			 "OBO_REL:results_in_secretion_of_granules_from|results_in_secretion_of_granules_from\n" + 
    			 "OBO_REL:results_in_patterning_for|results_in_patterning_for\n" + 
    			 "OBO_REL:results_in_movement_of|results_in_movement_of\n" + 
    			 "OBO_REL:results_in_localization_of|results_in_localization_of\n" + 
    			 "OBO_REL:results_in_increased_mass_of|results_in_increased_mass_of\n" + 
    			 "OBO_REL:results_in_increase_in_mass_of|results_in_increase_in_mass_of\n" + 
    			 "OBO_REL:results_in_decrease_in|results_in_decrease_in\n" + 
    			 "OBO_REL:results_in_increase_in|results_in_increase_in\n" + 
    			 "OBO_REL:results_in_remodeling_of|results_in_remodeling_of\n" + 
    			 "OBO_REL:results_in_fusion_of|results_in_fusion_of\n" + 
    			 "OBO_REL:results_in_division_of|results_in_division_of\n" + 
    			 "OBO_REL:results_in_complete_development_of|results_in_complete_development_of\n" + 
    			 "OBO_REL:results_in_developmental_progression_of|results_in_developmental_progression_of\n" + 
    			 "OBO_REL:results_in_formation_of|results_in_formation_of\n" + 
    			 "OBO_REL:results_in_addition_of|results_in_addition_of\n" + 
    			 "OBO_REL:results_in_addition_to|results_in_addition_to\n" + 
    			 "OBO_REL:results_in_removal_from|results_in_removal_from\n" + 
    			 "OBO_REL:results_in_removal_of|results_in_removal_of\n" + 
    			 "OBO_REL:results_in_determination_to|results_in_determination_to\n" + 
    			 "OBO_REL:results_in_commitment_to|results_in_commitment_to\n" + 
    			 "OBO_REL:results_in_specification_of|results_in_specification_of\n" + 
    			 "OBO_REL:results_in_determination_of|results_in_determination_of\n" + 
    			 "OBO_REL:results_in_creation_or_release_of|results_in_creation_or_release_of\n" + 
    			 "OBO_REL:results_in_creation_of|results_in_creation_of\n" + 
    			 "OBO_REL:results_in_transformation_to|results_in_transformation_to\n" + 
    			 "OBO_REL:results_in_change_to|results_in_change_to\n" + 
    			 "OBO_REL:results_in_breakdown_of|results_in_breakdown_of\n" + 
    			 "OBO_REL:results_in_binding_of|results_in_binding_of\n" + 
    			 "OBO_REL:results_in_acquisition_of_features_of|results_in_acquisition_of_features_of\n" + 
    			 "OBO_REL:occurs_in|occurs_in\n" + 
    			 "OBO_REL:mediated_by|mediated_by\n" + 
    			 "OBO_REL:increases_population_size_of|increases_population_size_of\n" + 
    			 "OBO_REL:has_input|has_input\n" + 
    			 "is_substituent_group_from|is substituent group from\n" + 
    			 "has_parent_hydride|has parent hydride\n" + 
    			 "has_functional_parent|has functional parent\n" + 
    			 "is_enantiomer_of|is enantiomer of\n" + 
    			 "OBO_REL:dependent_on|dependent_on\n" + 
    			 "OBO_REL:happens_in_response_to|happens_in_response_to\n" + 
    			 "OBO_REL:regulates_population_size_of|regulates_population_size_of\n" + 
    			 "OBO_REL:regulates_levels_of|regulates_levels_of\n" + 
    			 "OBO_REL:positively_regulates_timing_of|positively_regulates_timing_of\n" + 
    			 "OBO_REL:negatively_regulates_timing_of|negatively_regulates_timing_of\n" + 
    			 "OBO_REL:regulates_timing_of|regulates_timing_of\n" + 
    			 "OBO_REL:negatively_regulates|negatively_regulates\n" + 
    			 "OBO_REL:positively_regulates|positively_regulates\n" + 
    			 "OBO_REL:regulates|regulates\n" + 
    			 "OBO_REL:regulated_by|regulated_by\n" + 
    			 "OBO_REL:has_output|has_output\n" + 
    			 "OBO_REL:applicable_to_taxon|applicable_to_taxon\n" + 
    			 "OBO_REL:only_in_taxon|only_in_taxon\n" + 
    			 "OBO_REL:valid_for_taxon|valid_for_taxon\n" + 
    			 "OBO_REL:homologous_to|homologous_to\n" + 
    			 "OBO_REL:contains_variant_of|contains_variant_of\n" + 
    			 "OBO_REL:variant_of|variant_of\n" + 
    			 "OBO_REL:direct_copy_of|direct_copy_of\n" + 
    			 "OBO_REL:generically_specifies|specifies\n" + 
    			 "OBO_REL:generically_specified_by|specified_by\n" + 
    			 "OBO_REL:has_enabler|has_enabler\n" + 
    			 "OBOL:has_specific_outcome|has_specific_outcome\n" + 
    			 "OBOL:has_passive_participant|has_passive_participant\n" + 
    			 "OBOL:has_input_participant|has_input_participant\n" + 
    			 "OBOL:has_output_participant|has_output_participant\n" + 
    			 "OBOL:has_result|has_result\n" + 
    			 "OBO_REL:acts_on_population_of|acts_on_population_of\n" + 
    			 "OBOL:has_central_participant|has_central_participant\n" + 
    			 "OBOL:acts_on|acts_on\n" + 
    			 "OBO_REL:occurs_with|occurs_with\n" + 
    			 "OBOL:gives_rise_to|gives_rise_to\n" + 
    			 "OBO_REL:influenced_by|influenced_by\n" + 
    			 "OBO_REL:influences|influences\n" + 
    			 "OBO_REL:ends_earlier_than|ends_earlier_than\n" + 
    			 "OBO_REL:starts_earlier_than|starts_earlier_than\n" + 
    			 "OBO_REL:ends_during_or_before|ends_during_or_before\n" + 
    			 "OBO_REL:starts_during_or_after|starts_during_or_after\n" + 
    			 "OBO_REL:ends_at_beginning_of|ends_at_beginning_of\n" + 
    			 "OBO_REL:begins_at_end_of|begins_at_end_of\n" + 
    			 "OBO_REL:happens_during|happens_during\n" + 
    			 "OBO_REL:ends_during|ends_during\n" + 
    			 "OBO_REL:starts_during|starts_during\n" + 
    			 "OBO_REL:exists_during|exists_during\n" + 
    			 "OBO_REL:simultaneous_with|simultaneous_with\n" + 
    			 "OBO_REL:buds_from|buds_from\n" + 
    			 "OBO_REL:succeeds|succeeds\n" + 
    			 "OBO_REL:arises_from|arises_from\n" + 
    			 "OBO_REL:time_restricted_part_of|time_restricted_part_of\n" + 
    			 "OBO_REL:executed_in|executed_in\n" + 
    			 "OBO_REL:surrounds|surrounds\n" + 
    			 "OBO_REL:surrounded_by|surrounded_by\n" + 
    			 "OBO_REL:connected_to|connected_to\n" + 
    			 "OBO_REL:lacks_part|lacks_part\n" + 
    			 "OBOL:towards|OBOL:towards\n" + 
    			 "OBO_REL:disposition_of|disposition_of\n" + 
    			 "OBO_REL:has_disposition|has_disposition\n" + 
    			 "OBO_REL:exemplifies|exemplifies\n" + 
    			 "OBO_REL:depends_on|depends_on\n" + 
    			 "OBO_REL:specifically_inheres_in|specifically_inheres_in\n" + 
    			 "OBO_REL:multiply_inheres_in|multiply_inheres_in\n" + 
    			 "OBO_REL:has_phenotype|has_phenotype\n" + 
    			 "OBO_REL:towards|towards\n" + 
    			 "OBO_REL:realized_by|realized_by\n" + 
    			 "OBO_REL:realizes|realizes\n" + 
    			 "OBO_REL:has_role|has_role\n" + 
    			 "OBO_REL:role_of|role_of\n" + 
    			 "OBO_REL:has_function|has_function\n" + 
    			 "OBO_REL:function_of|function_of\n" + 
    			 "OBO_REL:has_quality|has_quality\n" + 
    			 "OBO_REL:quality_of|quality_of\n" + 
    			 "OBO_REL:bearer_of|bearer_of\n" + 
    			 "OBO_REL:inheres_in|inheres_in\n" + 
    			 "OBO_REL:agent_in|agent_in\n" + 
    			 "OBO_REL:has_agent|has_agent\n" + 
    			 "OBO_REL:participates_in|participates_in\n" + 
    			 "OBO_REL:has_participant|has_participant\n" + 
    			 "OBO_REL:transformed_into|transformed_into\n" + 
    			 "OBO_REL:adjacent_to|adjacent_to\n" + 
    			 "OBO_REL:contains|contains\n" + 
    			 "OBO_REL:contained_in|contained_in\n" + 
    			 "OBO_REL:relationship|relationship\n" + 
    			 "part_of|part of\n" + 
    			 "develops_from|develops from\n" + 
    			 "member_of|member_of\n" + 
    			 "has_part|has_part\n" + 
    			 "evidence_for_feature|evidence_for_feature\n" + 
    			 "derives_from|derives_from\n" + 
    			 "complete_evidence_for_feature|complete_evidence_for_feature\n" + 
    			 "OBO_REL:part_of|part_of\n" + 
    			 "OBO_REL:decreased_in_magnitude_compared_to|decreased_in_magnitude_compared_to\n" + 
    			 "OBO_REL:increased_in_magnitude_compared_to|increased_in_magnitude_compared_to\n" + 
    			 "OBO_REL:evolutionarily_derived_from|evolutionarily_derived_from\n" + 
    			 "OBO_REL:ancestral_copy_of|ancestral_copy_of\n" + 
    			 "OBO_REL:develops_from|develops_from\n" + 
    			 "OBO_REL:improper_part_of|improper_part_of\n" + 
    			 "OBO_REL:has_improper_part|has_improper_part\n" + 
    			 "OBO_REL:precedes|precedes\n" + 
    			 "OBO_REL:preceded_by|preceded_by\n" + 
    			 "OBO_REL:derived_into|derived_into\n" + 
    			 "OBO_REL:derives_from|derives_from\n" + 
    			 "OBO_REL:transformation_of|transformation_of\n" + 
    			 "OBO_REL:location_of|location_of\n" + 
    			 "OBO_REL:located_in|located_in\n" + 
    			 "OBO_REL:has_proper_part|has_proper_part\n" + 
    			 "OBO_REL:proper_part_of|proper_part_of\n" + 
    			 "OBO_REL:has_integral_part|has_integral_part\n" + 
    			 "OBO_REL:integral_part_of|integral_part_of\n" + 
    			 "OBO_REL:has_part|has_part\n");
    	
    	for (String s : allRelations.toString().split("\n")){
    		if (s.replace("_", " " ).contains(queryTerm)){
    			matches.append(s + "\n");
    		}
    	}
    	return matches;
    }
}
