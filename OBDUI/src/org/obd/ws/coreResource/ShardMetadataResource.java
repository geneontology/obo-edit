

package org.obd.ws.coreResource;


import org.obd.model.bridge.OBDJSONBridge;
import org.obd.model.stats.AggregateStatistic;
import org.obd.model.stats.AggregateStatisticCollection;
import org.obd.model.stats.CountStatistic;
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
public class ShardMetadataResource extends OBDResource {

	protected AggregateStatisticCollection stats;
 	protected String format;
 	protected String dataSource;
 	
    public String getFormat() {
		return format;
	}


	public void setFormat(String format) {
		this.format = format;
	}


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
    public ShardMetadataResource(Context context, Request request, Response response) {
        super(context, request, response);
        format = (String) request.getAttributes().get("format");
        this.dataSource = (String) request.getAttributes().get("dataSource");
        getStats();
        getVariants().add(new Variant(MediaType.TEXT_PLAIN));
    }

 
    /**
     * Finds the associated node.
     * 
     * @return The node found or null.
     */
    public AggregateStatisticCollection getStats() {
 
    	stats = getShard(this.dataSource).getSummaryStatistics();
   
        return stats;
    }

 
    @Override
    public Representation getRepresentation(Variant variant) {
        Representation result = null;
        
        if (format == null) {
        	format = "";
        }
    
        if (format.equals("json")) {
            result = new StringRepresentation(OBDJSONBridge.toJSON(stats).toString());
        }
        else {
          	// Creates a text representation
        	StringBuilder sb = new StringBuilder();
        	sb.append("<pre>");
          	sb.append("------------\n");
          	sb.append("OBD Shard Information\n");
           	sb.append("------------\n\n");
         	sb.append("<table>\n");
        	for (AggregateStatistic s : stats.getStats() ) {
    			if (s instanceof CountStatistic) {
    				CountStatistic c = (CountStatistic)s;
    				sb.append(s.getMeasuredEntity().toString()+" = "+
    						c.getCount());
    				sb.append("\n");
    			}
        	}
         	sb.append("</pre>");
            
         	                   	
         	result = new StringRepresentation(sb, MediaType.TEXT_HTML);
        }
        return result;
    }
    
 
    

}
