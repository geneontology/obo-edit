
package org.obd.ws.coreResource;


import org.obd.query.impl.MultiShard;
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
public class HomeResource extends OBDResource {


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
	public HomeResource(Context context, Request request, Response response) {
		super(context, request, response);
		getVariants().add(new Variant(MediaType.TEXT_PLAIN));
	}


	@Override
	public Representation getRepresentation(Variant variant) {
		Representation result = null;

		StringBuilder sb = new StringBuilder();
		sb.append("<html><body>");
		sb.append("<pre>");
		sb.append("\nData Sources:\n");
		sb.append("~~~~~~~~~~~~~~~~~~~~\n");
		for (String dataSource : ((OBDRestApplication)this.getApplication()).resourceMap.keySet()){
			sb.append(" - " + dataSource + " : \n");
			sb.append(" + -- Attached Shards : " + ((MultiShard)((OBDRestApplication)this.getApplication()).getShard(dataSource)).getShards().size() + "\n");
			for (String message : ((OBDRestApplication)this.getApplication()).getConfiguration().getSourceMessages().get(dataSource)){
				sb.append(" + -- " + message);
			}
		}
		sb.append("\n\nFormats:\n");
		sb.append("~~~~~~~~~~~~~~~~~~~~\n");
		sb.append(" - html : ultra basic information on resource\n");
		sb.append(" - json : OBD-JSON (not finalized)\n");
		sb.append(" - owl : [partial implementation]\n");
		sb.append(" - obo : [partial implementation] \n");
		sb.append(" - obdxml : TODO - next \n");
		sb.append(" - tab : tabular \n");
		sb.append("\n");
		sb.append("\n");
		
		sb.append("Paths:\n");
		sb.append("~~~~~~~~~~~~~~~~~~~~\n");
		for (String path : ((OBDRestApplication)this.getApplication()).getConfiguration().getPathResourceMap().keySet()){
			sb.append(this.example(path) + "\n");
		}
		
		sb.append("\n\nMapped Directories:\n");
		sb.append("~~~~~~~~~~~~~~~~~~~~\n");
		for (String path : ((OBDRestApplication)this.getApplication()).getConfiguration().getPathDirectoryMap().keySet()){
			sb.append(this.example(path) + "\n");
		}
		
		if (!((OBDRestApplication)this.getApplication()).getConfiguration().getPathMappingMessages().isEmpty()){
			sb.append("\n\nPath Mapping messages:\n");
			for (String message : (((OBDRestApplication)this.getApplication()).getConfiguration().getPathMappingMessages())){
				sb.append(" - " + message + "\n");
			}
		}
		
		sb.append("</pre>");
		sb.append("</body></html>");
		result = new StringRepresentation(sb, MediaType.TEXT_HTML);
		return result;
	}

	public String example(String path) {
		return "<a href=\"/" + this.getContextName() + path+"\">/" + this.getContextName() +path+"</a>";
	}

}
