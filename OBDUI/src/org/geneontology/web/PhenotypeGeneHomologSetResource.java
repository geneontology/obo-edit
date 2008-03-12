package org.geneontology.web;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.Collection;
import java.util.TreeMap;
import org.obd.model.HomologyView;
import org.obd.model.Node;
import org.restlet.resource.Variant;
import org.obd.ws.coreResource.NodeResource;
import org.restlet.Context;
import org.restlet.data.Request;
import org.restlet.data.Response;
import org.restlet.resource.Representation;

/**
 * Resource for a node
 * 
 * @author cjm
 */
public class PhenotypeGeneHomologSetResource extends NodeResource {

	protected String geneLabel;
	protected HomologyView hv;
	protected String dataSource;

	/**
	 * Constructor.
	 * 
	 * @param context
	 *            The parent context.
	 * @param request
	 *            The request to handle.
	 * @param response
	 *            The response to return.
	 */
	public PhenotypeGeneHomologSetResource(Context context, Request request, Response response) {
		super(context, request, response);
		geneLabel = (String) request.getAttributes().get("geneLabel");
		this.dataSource = (String) request.getAttributes().get("dataSource");
	}


	@Override
	public Representation getRepresentation(Variant variant) {
		

		hv = (HomologyView) readFromCache("hv",geneLabel);
		if (hv == null) {
			hv = new HomologyView(getShard(this.dataSource));
			Collection<Node> hsets = hv.getGeneHomolSetByGeneLabel(geneLabel);

			Node hset = hsets.iterator().next();
			hv.initializeFromGeneHomolSet(hset.getId());
			writeToCache("hv",geneLabel,hv);
		}
		else {
			hv.setShard(getShard(this.dataSource)); // we don't serialize the shard
		}



		TreeMap<String, Object> map = new TreeMap<String, Object>();
		map.put("homolview", hv);
		map.put("annotGraph", hv.getAnnotGraph());
		map.put("ontolGraph", hv.getOntolGraph());
		map.put("graph", hv.getOntolGraph()); // PageMacros assumes this is populated
		map.put("focusLabel",geneLabel);
		map.put("focusId",geneLabel);
		return getTemplateRepresentation("templates/PhenotypeGeneHomologSetView",map);
	}
	
	protected String cacheFilePath(String base, String id) {
		return base+"."+id+".ser";
	}
	
	protected void writeToCache(String base, String id, Serializable obj) {
		// serialize
		FileOutputStream fOut=null;
		ObjectOutputStream oOut=null;

		if (true) {
			try{
				fOut= new FileOutputStream(cacheFilePath(base,id));
				oOut = new ObjectOutputStream(fOut);
				oOut.writeObject(obj); //serializing
				System.out.println("written to "+fOut.toString());
			}
			catch(IOException e){
				e.printStackTrace();
			}
			finally{
				try {
					oOut.flush();
					oOut.close();
					fOut.close();
				} catch (IOException e1) {
					e1.printStackTrace();
				}
			}
		}
	}

	protected Object readFromCache(String base, String id) {
		FileInputStream fIn=null;
		ObjectInputStream oIn=null;
		Object obj = null;

		try{
			fIn= new FileInputStream(cacheFilePath(base,id));
			oIn = new ObjectInputStream(fIn);
			obj =  oIn.readObject();
			System.out.println("read from "+fIn.toString());		
		}
		catch(FileNotFoundException e){
			//  no need to report - expected for the first time round
		}
		catch(IOException e){
			e.printStackTrace();
			System.err.println("perhaps source code has changed?");
		}
		catch(ClassNotFoundException e){
			e.printStackTrace();
		}
		finally{
			try {
				if (oIn != null)
					oIn.close();
				if (fIn != null)
					fIn.close();
			} catch (IOException e1) {
				e1.printStackTrace();
			}
		}
		return obj;

	}


}



