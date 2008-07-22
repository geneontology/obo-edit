package org.obd.ws.coreResource.sorter;

import java.util.Comparator;

import org.obd.model.Node;


public class AutocompleteNodeSorter implements Comparator<Node>{

	private String pattern;
	
	public int compare(Node o1, Node o2) {
		String label1 = o1.getLabel();
		String label2 = o2.getLabel();
		
		if (label1==null){
			label1 = o1.getId();
		}
		if (label2==null){
			label2 = o2.getId();
		}
	
		int index1 = label1.indexOf(pattern);
		int index2 = label2.indexOf(pattern);
		
		if (index1<index2){
			return -1;
		} else if (index1>index2){
			return 1;
		} else {
			if (label1.length()<label2.length()){
				return -1;
			} else if (label1.length()>label2.length()){
				return 1;
			} else {
				return 0;
			}
		}
	}
	
	public void setPattern(String pattern){
		this.pattern = pattern;
	}
	
}