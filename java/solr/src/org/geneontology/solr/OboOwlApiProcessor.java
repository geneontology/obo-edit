package org.geneontology.solr;

import java.io.Reader;
import java.util.HashMap;
import java.util.Map;

import org.apache.solr.handler.dataimport.Context;
import org.apache.solr.handler.dataimport.DataSource;
import org.apache.solr.handler.dataimport.EntityProcessor;
import org.apache.solr.handler.dataimport.EntityProcessorBase;
import static org.apache.solr.handler.dataimport.DataImportHandlerException.SEVERE;
import static org.apache.solr.handler.dataimport.DataImportHandlerException.wrapAndThrow;


public class OboOwlApiProcessor extends EntityProcessorBase {

	  private boolean ended = false;

	  public OboOwlApiProcessor(){
		  System.out.println("********* I am created");
	  }
	  
	  
	  public void init(Context context) {
	    super.init(context);
	    ended = false;
	  }

	  public Map<String, Object> nextRow() {
	    
		  System.out.println("path*********************************************");
		  if (ended) return null;
	    DataSource<Reader> ds = context.getDataSource();
	    String path = context.getVariableResolver().replaceTokens(context.getEntityAttribute("path"));
	    System.out.println("path*********************************************");
	    Reader r = null;
	    try {
	      r = ds.getData(path);
	    } catch (Exception e) {
	      if (ABORT.equals(onError)) {
	        wrapAndThrow(SEVERE, e, "Exception reading url : " + path);
	      }
	      return null;
	    }
	    
	    /*
	    StringWriter sw = new StringWriter();
	    char[] buf = new char[1024];
	    while (true) {
	      int len = 0;
	      try {
	        len = r.read(buf);
	      } catch (IOException e) {
	        if (ABORT.equals(onError)) {
	          wrapAndThrow(SEVERE, e, "Exception reading url : " + url);
	        } else {
	          LOG.warn("IOException while reading from data source", e);
	          return null;
	        }
	      }
	      if (len <= 0) break;
	      sw.append(new String(buf, 0, len));
	    }*/
	    Map<String, Object> row = new HashMap<String, Object>();
	    row.put("id", "id123");
	    row.put("label", "label123");
	    row.put("source", "label123");
	    row.put("type", "label123");

	    ended = true;
	    return row;
	  }

	  public static final String PLAIN_TEXT = "plainText";

}
