package org.bbop.client.model;

import java.util.ArrayList;
import java.util.List;

import org.bbop.client.model.samples.TargetList;

public class TestData {
	
	public static List<TargetList> getTarget() {
		
		List<TargetList> targets = new ArrayList<TargetList>();
		
		targets.add(new TargetList("ALG6","NCBI_Gene:29929","Uniprot:Q9Y672"));
		targets.add(new TargetList("ALG3","NCBI_Gene:10195","Uniprot:Q92685"));
		targets.add(new TargetList("ALG2","NCBI_Gene:85365","Uniprot:Q9H553"));
		targets.add(new TargetList("ALG12","NCBI_Gene:79087","Uniprot:Q9BV10"));
		targets.add(new TargetList("ALB","NCBI_Gene:213","Uniprot:P02768"));
		targets.add(new TargetList("ACVR2B","NCBI_Gene:93","Uniprot:Q13705"));
		targets.add(new TargetList("ADH4","NCBI_Gene:127","Uniprot:P08319"));
		
		return targets;
		
	}

}
