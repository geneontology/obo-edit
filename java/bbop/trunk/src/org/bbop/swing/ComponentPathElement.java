package org.bbop.swing;

import java.util.StringTokenizer;

import org.apache.log4j.*;

public class ComponentPathElement {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ComponentPathElement.class);
	
	protected String id;
	protected String constraint;
	protected int index;
	
	protected ComponentPathElement(String id, String constraint, int index) {
		this.id = id;
		this.constraint = constraint;
		this.index = index;
	}
	
	public String getID() {
		return id;
	}
	
	public String getConstraint() {
		return constraint;
	}
	
	public int getIndex() {
		return index;
	}
	
	public static ComponentPathElement parse(String str) {
		String [] vals = new String[3];
		int i = 0;
		StringTokenizer tokenizer = new StringTokenizer(str, "|");
		while(tokenizer.hasMoreTokens()) {
			String token = tokenizer.nextToken();
			vals[i++] = token;
		}
		String id = vals[0];
		String constraint = vals[1];
		int index = -1;
		if (vals[2] != null) {
			index = Integer.parseInt(vals[2]);
		}
		return new ComponentPathElement(id, constraint, index);		
	}
}
