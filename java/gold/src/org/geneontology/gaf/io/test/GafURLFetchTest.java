package org.geneontology.gaf.io.test;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;

import junit.framework.TestCase;

import org.geneontology.gaf.hibernate.GafDocument;
import org.geneontology.gaf.hibernate.GafObjectsBuilder;
import org.geneontology.gaf.io.GafURLFetch;

public class GafURLFetchTest  extends TestCase {

	
	public void testFtpDir() throws Exception{
		GafURLFetch fetch = new GafURLFetch("ftp://ftp.geneontology.org/go/gene-associations/");
		fetch.connect();
		while(fetch.hasNext()){
			InputStream is = (InputStream)fetch.next();
			
			BufferedReader reader = new BufferedReader( new InputStreamReader(is));
			
			System.out.println( reader.readLine());
			
			reader.close();
			is.close();
			fetch.completeDownload();
			
		}
	}

	public void testFtpFile() throws Exception{
		GafURLFetch fetch = new GafURLFetch("ftp://ftp.geneontology.org/go/gene-associations/gene_association.PAMGO_Atumefaciens.gz");
		fetch.connect();
		while(fetch.hasNext()){
			InputStream is = (InputStream)fetch.next();
			
			GafObjectsBuilder  builder = new GafObjectsBuilder();

			Reader reader = new InputStreamReader(is);
			
			GafDocument doc = builder.buildDocument(reader, fetch.getCurrentGafFile(), fetch.getCurrentGafFilePath());
			
			
			System.out.println( doc.getBioentities());
			
			reader.close();
			is.close();
			fetch.completeDownload();
			
		}
	}
	
	public void testHttpFile() throws Exception{
		GafURLFetch fetch = new GafURLFetch("http://www.geneontology.org/gene-associations/gene_association.PAMGO_Ddadantii.gz");
		fetch.connect();
		while(fetch.hasNext()){
			InputStream is = (InputStream)fetch.next();
			
			GafObjectsBuilder  builder = new GafObjectsBuilder();

			Reader reader = new InputStreamReader(is);
			
			GafDocument doc = builder.buildDocument(reader, fetch.getCurrentGafFile(), fetch.getCurrentGafFilePath());
			
			
			System.out.println( doc.getBioentities());
			
			reader.close();
			is.close();
			fetch.completeDownload();
			
		}
	}
	
	
	
}
