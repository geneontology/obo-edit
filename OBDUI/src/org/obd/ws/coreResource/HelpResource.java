
package org.obd.ws.coreResource;



import org.obd.ws.restServlet.OBDRestApplication;
import org.restlet.Context;
import org.restlet.data.MediaType;
import org.restlet.data.Request;
import org.restlet.data.Response;
import org.restlet.resource.Representation;
import org.restlet.resource.StringRepresentation;
import org.restlet.resource.Variant;

/**
 * Resource for a node
 */
public class HelpResource extends OBDResource {

	/**
     * Constructor.
     * 
     * @param context
     *                The parent context.
     * @param request
     *                The request to handle.
     * @param response
     *                The response to return.
     */
    public HelpResource(Context context, Request request, Response response) {
        super(context,request,response);
        getVariants().add(new Variant(MediaType.TEXT_PLAIN));
    }

 
    /**
     * Finds the associated node.
     * 
     * @return The node found or null.
     */


 
    @Override
    public Representation getRepresentation(Variant variant) {
    	Representation result = null;

		StringBuilder sb = new StringBuilder();
		sb.append("<pre>");
		sb.append("------------\n");
		sb.append("OBD\n");
		sb.append("------------\n\n");
		sb.append("This is the RESTful interface to an OBD shard\n");
		sb.append("All resources in this shard should be accessible via a URL\n");
		sb.append("For metadata on this shard see: "+example("/html/metadata/")+" (slow)\n");
		sb.append("\n");
		sb.append("\n");

		sb.append("Use Cases:\n");
		sb.append("~~~~~~~~~~\n\n");
		sb.append("  <a href='/usecases/index.html'>Use Cases</a> \n");
		sb.append("\n");
		sb.append("\n");
		sb.append("Data Sources:\n");
		sb.append("~~~~~~~~~~~~~~~~~~~~\n");
		for (String dataSource : ((OBDRestApplication)this.getApplication()).resourceMap.keySet()){
			sb.append(" - " + dataSource + "\n");
		}

		sb.append("\n\nGET Patterns Implemented:\n");
		sb.append("~~~~~~~~~~~~~~~~~~~~\n\n");

		sb.append("  /{dataSource}/{format}/nodes/{id}\n");
		sb.append("     A Node : can be class instance or relation\n");
		sb.append("     Example: "+example("/html/node/CL:0000148")+"\n");
		sb.append("     Example: "+example("/json/node/MP:0001306")+"\n");
		sb.append("     Example: "+example("/json/node/entrezgene:2138")+"\n");
		sb.append("     Example: "+example("/obdxml/node/WB%3AWBGene00001377")+"\n");
		sb.append("     Example: "+example("/json/node/OMIM:601653.0008")+"\n");
		sb.append("     Example: "+example("/html/node/PATO:0000963^OBO_REL:inheres_in(FMA:58238)")+"\n");
		sb.append("     Example: "+example("/owl/node/CL:0000100")+"\n");
		sb.append("     TODO: concatenate multiple using +\n");
		sb.append("\n");

		sb.append("  /{dataSource}/{format}/nodes/{id}/statements\n");
		sb.append("     All statements ABOUT a node\n");
		sb.append("     Synonymous with: {format}/node/{id}/statement/about\n");
		sb.append("     Example: "+example("/html/node/CL:0000148/statements")+"\n");
		sb.append("     Example: "+example("/json/node/MP:0001306/statements")+"\n");
		sb.append("     Example: "+example("/owl/node/MP:0001306/statements")+"\n");
		sb.append("     Example: "+example("/obo/node/MP:0001306/statements")+"\n");
		sb.append("     Example: "+example("/json/node/OMIM:601653.0008/statements")+"\n");
		sb.append("     Example: "+example("/json/node/OWB%3AWBGene00001377/statements")+"\n");

		sb.append("\n");


		sb.append("  /{dataSource}/{format}/node/{id}/statements/{aspect}/\n");
		sb.append("  /{dataSource}/{format}/node/{id}/statements/{aspect}/{relation}\n");
		sb.append("     All statements concerning a node in some way, optionally filtered by relation\n");
		sb.append("     Aspect = about OR to OR all\n");
		sb.append("     Example: "+example("/html/node/CL:0000148/statements/about")+"\n");
		sb.append("     Example: "+example("/html/node/CL:0000148/statements/to")+"\n");
		sb.append("     Example: "+example("/html/node/CL:0000148/statements/annotations")+"\n");
		sb.append("     Example: "+example("/html/node/PATO:0000963^OBO_REL:inheres_in(FMA:58238)/statements/to")+"\n");
		sb.append("     Example: "+example("/html/node/CL:0000148/statements/to/inheres_in")+"\n");
		sb.append("     Example: "+example("/html/node/CL:0000148/statements/about/OBO_REL:is_a")+"\n");
		sb.append("     Example: "+example("/html/node/CL:0000148/statements/all")+"\n");
		sb.append("     Example: "+example("/json/node/entrezgene:2138/statements/all")+"\n");
		sb.append("     Example: "+example("/json/node/MP:0005099/statements/all")+"\n");
		sb.append("     Example: "+example("/json/node/MP:0002877/statements/annotations")+"\n");
		sb.append("     Example: "+example("/obo/node/PATO:0000963^OBO_REL:inheres_in(FMA:58238)/statements/to")+"\n");
		sb.append("     Example: "+example("/html/node/omim_phenotype_fb/statements/source/")+"\n");
		sb.append("     Example: "+example("/obo/node/CL:0000100/statements/annotations")+"\n");

		sb.append("\n");

		sb.append("  /{dataSource}/{format}/node/{id}/graph\n");
		sb.append("  /{dataSource}/{format}/node/{id}/graph/{aspect}\n");
		sb.append("     Graph around node; deductive closure to roots\n");
		sb.append("     Example: "+example("/json/node/MP:0001306/graph")+"\n");
		sb.append("     Example: "+example("/html/node/MP:0001306/graph")+"\n");
		sb.append("     Example: "+example("/obo/node/MP:0001306/graph")+"\n");
		sb.append("     Example: "+example("/html/node/MP:0001306/graph")+"\n");
		sb.append("     Example: "+example("/html/node/ZFIN:ZDB-GENO-980202-1557/graph")+"\n");
		sb.append("     Example: "+example("/html/node/MP:0001306/graph")+"\n");
		sb.append("     Example: "+example("/html/node/Sox10%3Ctm3%28Sox8%29Weg%3E%2FSox10%3Ctm3%28Sox8%29Weg%3E/graph")+"\n");
		sb.append("     Example: "+example("/html/node/CL:0000100/graph/annotations")+"\n");
		sb.append("\n");
		sb.append("  /{dataSource}/{format}/node/{id}/description\n");
		sb.append("     Composite description (class expression, post-composition)\n");
		sb.append("     Example: "+example("/json/node/MP:0001306/description")+"\n");
		sb.append("     Example: "+example("/obo/node/MP:0001306/description")+"\n");
		sb.append("     Example: "+example("/owl/node/MP:0001306/description")+"\n");
		sb.append("     Example: "+example("/html/node/MP:0004176/description")+"\n");
		sb.append("     Example: "+example("/html/node/PATO:0000963^OBO_REL:inheres_in(FMA:58238)/description")+"\n");
		sb.append("     Example: "+example("/obo/node/PATO:0000963^OBO_REL:inheres_in(FMA:58238)/description")+"\n");
		sb.append("     Example: "+example("/owl/node/PATO:0000963^OBO_REL:inheres_in(FMA:58238)/description")+"\n");
		sb.append("\n");

		sb.append("  /{dataSource}/{format}/node/{id}/blast\n");
		sb.append("     Find similar annotated entity nodes, based on annotations\n");
		sb.append("     Example: "+example("/json/node/OMIM:601653.0008/blast")+"\n");
		sb.append("     Example: "+example("/json/node/ZFIN:ZDB-GENO-980410-322/blast")+"\n");
		sb.append("     Example: "+example("/json/node/ZFIN:ZDB-GENO-980202-1557/blast")+" -- what is similar to eya1?\n");
		sb.append("     Example: "+example("/json/node/Sox10%3Ctm3%28Sox8%29Weg%3E%2FSox10%3Ctm3%28Sox8%29Weg%3E/blast")+" -- what is similar to mouse SOX10?\n");

		sb.append("\n");

		sb.append("  /{dataSource}/{format}/node/{id}/annotation-graph\n");
		sb.append("     Graph around annotations to node\n");             
		sb.append("     TODO\n");
		sb.append("     Example: "+example("/json/node/MP:0001306/annotation-graph")+"\n");
		sb.append("\n");

		sb.append("  /{dataSource}/{format}/search/{searchTerm}\n");
		sb.append("     Matching nodes (defaults to exact match)\n");
		sb.append("     Example: "+example("/html/search/matches/eye")+"\n");
		sb.append("     Example: "+example("/html/search/matches/Eye")+"\n");
		sb.append("     Example: "+example("/html/search/matches/eya1")+"\n");
		sb.append("     Example: "+example("/html/search/matches/EYA1")+"\n");
		sb.append("     Example: "+example("/html/search/matches/EYA1+SHH")+"\n"); // TODO: quote phrases
		sb.append("     Example: "+example("/html/search/matches/cell")+"\n");
		sb.append("\n");

		sb.append("  /{dataSource}/{format}/search/{comparisonOperator}/{searchTerm}\n");
		sb.append("     Matching nodes; operator = starts_with OR contains\n");
		sb.append("     Example: "+example("/html/search/contains/melanocyte")+"\n");
		sb.append("     Example: "+example("/json/search/contains/melanocyte")+"\n");
		sb.append("     Example: "+example("/obo/search/contains/leukocyte")+"\n");
		sb.append("     Example: "+example("/owl/search/contains/melanocyte")+"\n");
		sb.append("     Example: "+example("/html/search/starts_with/eye")+"\n");
		sb.append("     Example: "+example("/obo/search/starts_with/eye")+"\n");
		sb.append("     Example: "+example("/html/search/contains/eye")+"\n");
		sb.append("     Example: "+example("/html/search/starts_with/eya")+"\n");
		sb.append("     Example: "+example("/html/search/starts_with/OMIM:601653")+"\n");
		sb.append("\n");

		sb.append("  /{dataSource}/{format}/search/[{aspect}]{comparisonOperator}/{searchTerm}   -- TODO; eg search by synonym\n");
		sb.append("     TODO\n");
		sb.append("\n");

		sb.append("  /{dataSource}/{format}/nodes/{id1+id2+...+idN} -- TODO\n");
		sb.append("     A list of nodes -- TODO\n");
		sb.append("     Example: "+example("/html/nodes/CL:0000148+CL:0000541")+"\n");
		sb.append("     Example: "+example("/json/nodes/MP:0001306+FMA:58241")+"\n");
		sb.append("\n");
		sb.append("  /{dataSource}/{format}/nodes/{id1+id2+...+idN}/statements -- TODO\n");
		sb.append("     Statements involving the union of above IDs\n");
		sb.append("     Example: "+example("/html/nodes/CL:0000148+CL:0000541/statements")+"\n");
		sb.append("     Example: "+example("/json/nodes/MP:0001306+FMA:58241/statements")+"\n");
		sb.append("\n");
		sb.append("  /{dataSource}/{format}/nodes/{id1+id2+...+idN}/graph -- TODO\n");
		sb.append("     Graph involving the union of above IDs\n");
		sb.append("\n");
		sb.append("  /{dataSource}/{format}/nodes/{id1^id2^...^idN}/graph -- TODO\n");
		sb.append("     Graph involving the union of above IDs\n");
		sb.append("\n");
		sb.append("  /{dataSource}/{format}/nodes/{id1+id2+...+idN}/annotations -- TODO\n");
		sb.append("     Annotations to the union of above IDs\n");
		sb.append("\n");
		sb.append("  /{dataSource}/{format}/nodes/{id1^id2^...^idN}/annotations -- TODO\n");
		sb.append("     Annotations to (entities with one of all of?) intersection of above IDs\n");
		sb.append("\n");


		sb.append("  / - this page\n");
		sb.append("  /{dataSource}/{format}/metadata\n");
		sb.append("  /{dataSource}/{format}/source/{id}   -- TODO\n");
		sb.append("  /{dataSource}/{format}/source/{id}/nodes   -- TODO\n");
		sb.append("  /{dataSource}/{format}/source/{id}/graph   -- TODO\n");
		sb.append("  /{dataSource}/{format}/source/{id}/summary   -- TODO\n");
		sb.append("  /{dataSource}/{format}/relation/{id}   -- TODO\n");
		sb.append("  /{dataSource}/{format}/nodes-by-query/{obdQuery}   -- TODO\n");
		sb.append("  /{dataSource}/{format}/nodes-by-query/{obdQuery}/graph   -- TODO\n");
		sb.append("  /{dataSource}/{format}/hits-by-co-annotated/{id}   -- TODO\n");
		sb.append("  /{dataSource}/{format}/hits-by-semantic-similarity/{id}   -- TODO\n");
		sb.append("\n");

		sb.append("\n");

		sb.append("Formats:\n");
		sb.append("~~~~~~~~~~~~~~~~~~~~\n\n");
		sb.append(" - html : ultra basic information on resource\n");
		sb.append(" - json : OBD-JSON (not finalized)\n");
		sb.append(" - owl : [partial implementation]\n");
		sb.append(" - obo : [partial implementation] \n");
		sb.append(" - obdxml : TODO - next \n");
		sb.append(" - tab : tabular \n");
		sb.append("\n");
		sb.append("\n");

		sb.append("Controlling number of results:\n");
		sb.append("~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n");
		sb.append(" [TODO]\n");
		sb.append("Fetching counts:\n");
		sb.append("  Append /count -- EXAMPLE: /json/node/CL:0000000/statements/count:\n");
		sb.append("Use query params for limits/cursors:\n");
		sb.append("    ?limit=100:\n");
		sb.append("    ?from=200&limit=100:\n");
		sb.append("\n");

		sb.append("PUT Patterns:\n");
		sb.append("~~~~~~~~~~~~~~~~~~~~\n\n");
		sb.append(" class enrichment TODO\n");
		sb.append(" upload TODO\n");
		sb.append(" mapping TODO\n");
		sb.append("\n");


		sb.append("<pre>");
		result = new StringRepresentation(sb, MediaType.TEXT_HTML);
		return result;
	}

	public String example(String path) {
		String testResource = (String) ((OBDRestApplication)this.getApplication()).getResourceMap().keySet().toArray()[0];
		return "<a href=\"/" + this.getContextName() + "/" + testResource  + path+"\">/" + this.getContextName() + "/" + testResource +path+"</a>";
	}
    

}
