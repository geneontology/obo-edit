package org.geneontology.gaf.io;

import java.io.IOException;
import java.io.InputStream;
import java.net.SocketException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.zip.GZIPInputStream;
import org.apache.commons.net.ftp.FTPClient;
import org.apache.commons.net.ftp.FTPFile;
import org.apache.log4j.Logger;
import org.geneontology.gaf.io.test.GafURLFetchTest;
import org.geneontology.web.services.GafDbOperationsService;

/**
 * This class builds the {@link InputStream} objects for the http/ftp URLs.
 * The next method returns the input streams object. If the protocol is ftp  then
 * completeDownload is to be called after reading the input the stream.
 * See {@link GafURLFetchTest} and {@link GafDbOperationsService} for use.
 * @author Shahid Manzoor
 *
 */
public class GafURLFetch implements Iterator {

	private static Logger LOG = Logger.getLogger(GafURLFetch.class);
	private static boolean DEBUG = LOG.isDebugEnabled();
	
	private String url;
	
	private FTPFile[] files;
	
	private FTPClient ftpClient;
	
	private boolean isConnected;
	
	private int counter;
	
	private URL httpURL;
	
	public GafURLFetch(String url){
		this.url =  url ;
		ftpClient = new FTPClient();
		counter = 0;
	}

	public boolean hasNext() {
		if(!this.isConnected){
			throw new IllegalStateException("Not connected to the ftp client");
		}
		
		if(httpURL != null)
			return true;
		
		if(counter>=files.length)
			return false;
		
		if( files[counter].isDirectory()){
			counter++;
			return hasNext();
		}else{
			return true;
		}
		
		
			
	}

	public Object next() {
		if(!hasNext()){
			throw new NoSuchElementException();
		}
		
		try{
			if(this.httpURL != null){
				InputStream is = this.httpURL.openStream();
				
				if(url.endsWith(".gz")){
					is = new GZIPInputStream(is);
				}
				this.httpURL = null;
				return is;
			}else{
				
				String file = files[counter++].getName();
				
				if(DEBUG)
					LOG.debug("Returning input stream for the file: " + file);
				
				InputStream is = ftpClient.retrieveFileStream(file);
				
				if(file.endsWith(".gz")){
					is = new GZIPInputStream(is);
				}
				
				return is;
			}
		}catch(IOException ex){
			throw new RuntimeException(ex);
		}
		
	}
	
	public void completeDownload() throws IOException{
		if(this.url.startsWith("ftp:"))
			ftpClient.completePendingCommand();
	}

	public void remove() {
	}

	public boolean connect() throws SocketException, IOException, URISyntaxException{
		if(this.isConnected)
			return true;
		
		URI uri = new URI(url);
		
		if(this.url.startsWith("http://")){
			this.httpURL = uri.toURL();
			this.isConnected = true;
			this.files = new FTPFile[]{};
			return true;
		}
		
		this.isConnected = false;
		String host = uri.getHost();
		int port = uri.getPort();
		
		if(port == -1)
			port = 21;
		
		ftpClient.connect( host, port );
		ftpClient.login("anonymous", "");
		ftpClient.enterLocalActiveMode();
		
		this.files = ftpClient.listFiles( uri.getPath() );

		String path = uri.getPath();
		
		if(this.files.length==1){
			path = path.substring(0, path.lastIndexOf("/")) + "/";
			
		}
		
		ftpClient.changeWorkingDirectory(path);
		
		this.isConnected = true;
		
		return true;
	}
	
}
