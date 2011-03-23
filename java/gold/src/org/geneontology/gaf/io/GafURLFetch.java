package org.geneontology.gaf.io;

import java.io.IOException;
import java.io.InputStream;
import java.net.SocketException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Iterator;
import java.util.zip.GZIPInputStream;

import org.apache.commons.net.ftp.FTPClient;
import org.apache.commons.net.ftp.FTPReply;

public class GafURLFetch implements Iterator {

	private String url;
	
	private String[] files;
	
	private FTPClient ftpClient;
	
	private boolean isConnected;
	
	private int counter;
	
	private URL httpURL;
	
	public GafURLFetch(String url){
		this.url = url;
		ftpClient = new FTPClient();
		counter = 0;
	}

	public boolean hasNext() {
		if(!this.isConnected){
			throw new IllegalStateException("Not connected to the ftp client");
		}
		
		if(httpURL != null)
			return true;
		
		return counter<files.length;
	}

	public Object next() {
		if(!hasNext()){
			return null;
		}
		
		try{
			if(this.httpURL != null){
				InputStream is = this.httpURL.openStream();
				
				if(url.endsWith(".gz")){
					is = new GZIPInputStream(is);
				}
				
				return is;
			}else{
				
				String file = files[counter++];
				
				InputStream is = ftpClient.retrieveFileStream(file);
				
				if(url.endsWith(".gz")){
					is = new GZIPInputStream(is);
				}
				
				return is;
			}
		}catch(IOException ex){
			throw new RuntimeException(ex);
		}
		
	}

	public void remove() {
	}

	public boolean connect() throws URISyntaxException, SocketException, IOException{
		if(this.isConnected)
			return true;
		
		URI uri = new URI(url);
		
		if(this.url.startsWith("http://")){
			this.httpURL = uri.toURL();
			this.isConnected = true;
			this.files = new String[]{};
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
		
		/*int reply = ftpClient.getReply();

		
		if(!FTPReply.isPositiveIntermediate(reply)){
			ftpClient.disconnect();
			return false;
		}*/
		
		
		/*if(url.endsWith(".gaf")){
			String file = url.substring( url.lastIndexOf("/"));
			this.files = new String[]{file};
		}else{*/
			
		this.files = ftpClient.listNames( uri.getPath() );

		this.isConnected = true;
		
		return true;
	}
	
}
