package org.obd.ws.coreResource.sorter;

import java.util.Comparator;

import org.obd.model.Node;


public class NodeLabelComparator implements Comparator<Node>{

	public int compare(Node o1, Node o2) {
		String label1 = o1.getLabel();
		String label2 = o2.getLabel();
		
		if (label1==null){
			label1 = o1.getId();
		}
		if (label2==null){
			label2 = o2.getId();
		}
		
		return label1.compareToIgnoreCase(label2);
		
	}
	
}