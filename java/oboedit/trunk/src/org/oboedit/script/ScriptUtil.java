package org.oboedit.script;

import java.io.BufferedReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.bbop.io.IOUtil;

/**
 * This utility will include a number of Java utility methods for scripts in a 
 * future version of OBO-Edit
 * 
 * @author jrichter
 */
import org.apache.log4j.*;

public class ScriptUtil {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ScriptUtil.class);
	public Set createSet() {
		return new HashSet();
	}

	public List createLinkedList() {
		return new LinkedList();
	}
	
	public List createList() {
		return new ArrayList();
	}
	
	public Map createMap() {
		return new HashMap();
	}
	
	public BufferedReader getFileReader(String file) {
		try {
			return new BufferedReader(new InputStreamReader(IOUtil.getStream(file)));
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}
	}
	
	public PrintWriter getFileWriter(String file) {
		try {
			return new PrintWriter(new FileWriter(file));
		} catch (IOException ex) {
			return null;
		}
	}
}
