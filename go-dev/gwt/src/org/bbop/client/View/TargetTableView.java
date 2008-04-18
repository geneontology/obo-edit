package org.bbop.client.View;

import java.util.ArrayList;
import java.util.Date;

import net.mygwt.ui.client.Style;
import net.mygwt.ui.client.widget.Button;
import net.mygwt.ui.client.widget.table.DateTimeCellRenderer;
import net.mygwt.ui.client.widget.table.Table;
import net.mygwt.ui.client.widget.table.TableColumn;
import net.mygwt.ui.client.widget.table.TableColumnModel;
import net.mygwt.ui.client.widget.table.TableItem;

import org.bbop.client.Listener.RefGenomeViewListenerI;


import com.google.gwt.user.client.ui.CheckBox;

public class TargetTableView {

	private RefGenomeViewListenerI refgListener;
	private RefGenomeView mainView;
	
	//private WidgetContainer summaryWidget;
	private TableColumnModel targetColModel;
	private Table targetTbl;
	private TableColumn[] targetCols;
	private ArrayList testData;
	private Date currDate;
	
	public TargetTableView (RefGenomeViewListenerI listener, RefGenomeView parent) {
		refgListener = listener;
		mainView = parent;
		currDate = new Date();
	}
	
	public void createView() {
		
		// This is where it should receive data from server
		targetCols = new TableColumn[7];
		
		targetCols[0] = new TableColumn(" ",.5f);
		targetCols[0].setMinWidth(10);
		targetCols[0].setMaxWidth(20);
		
		targetCols[1] = new TableColumn("Gene symbol",.10f);
		targetCols[1].setMinWidth(20);
		targetCols[1].setMaxWidth(100);
		
		targetCols[2] = new TableColumn("Gene id",.10f);
		targetCols[2].setMinWidth(20);
		targetCols[2].setMaxWidth(100);
		
		targetCols[3] = new TableColumn("Protein id",.10f);
		targetCols[3].setMinWidth(20);
		targetCols[3].setMaxWidth(100);
		
		targetCols[4] = new TableColumn("Completion date",.10f);
		targetCols[4].setMinWidth(20);
		targetCols[4].setMaxWidth(100);
		targetCols[4].setRenderer(new DateTimeCellRenderer("MMM yy"));
		
		targetCols[5] = new TableColumn(" ",.20f);
		targetCols[5].setMinWidth(20);
		targetCols[5].setMaxWidth(180);
		
		targetCols[6] = new TableColumn("Curation",.20f);
		targetCols[6].setMinWidth(20);
		targetCols[6].setMaxWidth(180);
		
		targetColModel = new TableColumnModel(targetCols);
		targetTbl = new Table(Style.MULTI,targetColModel);
		targetTbl.setBorders(true);
		
		setTestData();
		
		for(int i = 0; i < testData.size(); i++) {
			Object[] targetData = new Object[7];
			String[] data = (String[]) testData.get(i);
			CheckBox cb = new CheckBox();
			Button orthoBtn = new Button("View ortholog");
			Button reportBtn = new Button("Curation report");
			orthoBtn.setIconStyle("icon-view");
			reportBtn.setIconStyle("icon-report-go");
			
			cb.setName(Integer.toString(i));
			targetData[0] = cb;
			targetData[1] = data[0];
			targetData[2] = data[1];
			targetData[3] = data[2];
			targetData[4] = currDate;
			targetData[5] = orthoBtn;
			targetData[6] = reportBtn;
			
			targetTbl.add(new TableItem(targetData));
			
		}
		
		
		
	}
	
	public void setAttr() {
		
	}
	
	public void addObservers () {
		
	}
	
	public Table getView() {
		return targetTbl;
	}
	
	
	public void setTestData() {
		testData = new ArrayList();
		testData.add(new String[] {"ALG6","NCBI_Gene:29929","Uniprot:Q9Y672"});
		testData.add(new String[] {"ALG3","NCBI_Gene:10195","Uniprot:Q92685"});
		testData.add(new String[] {"ALG2","NCBI_Gene:85365","Uniprot:Q9H553"});
		testData.add(new String[] {"ALG12","NCBI_Gene:79087","Uniprot:Q9BV10"});
		testData.add(new String[] {"ALDH5A1","NCBI_Gene:7915","Uniprot:P51649"});
		testData.add(new String[] {"ALB","NCBI_Gene:213","Uniprot:P02768"});
		testData.add(new String[] {"ACVR1B","NCBI_Gene:91","Uniprot:P36896"});
		testData.add(new String[] {"ACVR2B","NCBI_Gene:93","Uniprot:Q13705"});
		testData.add(new String[] {"ADH1A","NCBI_Gene:124","Uniprot:P07327"});
		testData.add(new String[] {"ADH4","NCBI_Gene:127","Uniprot:P08319"});
	}
}
