package org.geneontology.gaf.io;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.SocketException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.zip.GZIPInputStream;

import org.apache.commons.io.IOUtils;
import org.apache.commons.net.ftp.FTPClient;
import org.apache.commons.net.ftp.FTPFile;
import org.apache.log4j.Logger;
import org.geneontology.conf.GeneOntologyManager;
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
	

	private String currentGafFile;
	
	private String currentGafFilePath;
	
	private String host;
	
	private int port;
	
	private String path;
	
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

		this.currentGafFilePath = this.url;
		
		try{
			if(this.httpURL != null){
				LOG.info("Reading URL :" + httpURL);
				InputStream is = this.httpURL.openStream();
			
				int index = this.httpURL.toString().lastIndexOf('/');
				
				String file = this.httpURL.toString().substring(index+1);
				File downloadLocation = new File(GeneOntologyManager.getInstance().getGafUploadDir(), "tmp-"+file);
				OutputStream out = 
						new FileOutputStream(downloadLocation);

				IOUtils.copy(is, out);
				out.close();
				
				is = new FileInputStream(downloadLocation);
				
				if(url.endsWith(".gz")){
					is = new GZIPInputStream(is);
				}
				
				this.currentGafFile = this.currentGafFilePath.substring(this.currentGafFilePath.lastIndexOf("/")+1);
				
				this.httpURL = null;
				return is;
			}else{
				
				String file = files[counter++].getName();
		
				this.currentGafFile = file;
				
				
				if(!this.currentGafFilePath.endsWith(file))
					currentGafFilePath += file;
				
				
				LOG.info("Returning input stream for the file: " + file);
	
				_connect();
				
				ftpClient.changeWorkingDirectory(path);

				InputStream is = ftpClient.retrieveFileStream(file);
				File downloadLocation = new File(GeneOntologyManager.getInstance().getGafUploadDir(), file);
				OutputStream out = 
						new FileOutputStream(downloadLocation);
				
				
				/*int b;
				while((b=is.read()) != -1){
					out.write(b);
				}*/
				
				
				IOUtils.copy(is, out);
				
				out.close();
				
				System.out.println("Download complete.....");
				//ftpClient.completePendingCommand();

				is = new FileInputStream(downloadLocation);
				
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
		/*if(this.url.startsWith("ftp:"))
			ftpClient.completePendingCommand();*/
	}

	public void remove() {
	}

	
	private void _connect() throws SocketException, IOException{
		try{
			ftpClient.disconnect();
		}catch(Exception ex){
			
		}
		
		
		ftpClient.connect( host, port );
		ftpClient.login("anonymous", "");
		ftpClient.enterLocalActiveMode();
		
	}
	
	public boolean connect() throws SocketException, IOException, URISyntaxException{
		if(this.isConnected)
			return true;
		
		URI uri = new URI(url);
		
		this.currentGafFile = null;
		
		if(this.url.startsWith("http://") || this.url.startsWith("file:")){
			this.httpURL = uri.toURL();
			this.isConnected = true;
			this.files = new FTPFile[]{};
			return true;
		}
		
		this.isConnected = false;
		this.host = uri.getHost();
		this.port = uri.getPort();
		
		if(port == -1)
			port = 21;
		
		
		_connect();
		
		this.files = ftpClient.listFiles( uri.getPath() );

		path = uri.getPath();
		
		if(this.files.length==1){
			path = path.substring(0, path.lastIndexOf("/")) + "/";
			
		}
		
		
		this.isConnected = true;
		
		return true;
	}

	public String getCurrentGafFile() {
		return currentGafFile;
	}

	public String getCurrentGafFilePath() {
		return currentGafFilePath;
	}

}
