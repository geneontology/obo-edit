package org.bbop.client.View;



import org.bbop.client.Listener.RefGenomeViewListenerI;

import net.mygwt.ui.client.Style;


import net.mygwt.ui.client.widget.table.Table;
import net.mygwt.ui.client.widget.table.TableColumn;
import net.mygwt.ui.client.widget.table.TableColumnModel;
import net.mygwt.ui.client.widget.table.TableItem;

public class SummaryTableView {
	private RefGenomeViewListenerI refgListener;
	private RefGenomeView mainView;
	
	//private WidgetContainer summaryWidget;
	private TableColumnModel summColModel;
	private Table summTbl;
	private TableColumn[] summCols;
	
	public SummaryTableView(RefGenomeViewListenerI listener,RefGenomeView parent){
		refgListener = listener;
		mainView = parent;
		
		//summaryWidget = new WidgetContainer();
		setAttr();
		addObservers();
		
	}
	
	public void createView() {
		summColModel = new TableColumnModel(summCols);
		
		summTbl = new Table(Style.MULTI, summColModel);
		summTbl.setBorders(false);
		summTbl.add(new TableItem(new String[]{"Worm","200","300","189"}));
		summTbl.add(new TableItem(new String[]{"Fly","185","19","160"}));
		summTbl.add(new TableItem(new String[]{"Human","268","30","100"}));
		
			
		//summaryWidget.setLayout(new FillLayout(10));
		//summaryWidget.add(summTbl);
	}
	
	public void addObservers() {
		
	}
	
	public void setAttr() {
		
		
		
		summCols = new TableColumn[4];
		
		summCols[0] = new TableColumn("Species",.30f);
		summCols[0].setMinWidth(30);
		summCols[0].setMaxWidth(200);
		
		summCols[1] = new TableColumn("Homologs",.25f);
		summCols[1].setMinWidth(30);
		summCols[1].setMaxWidth(200);
		
		summCols[2] = new TableColumn("Not Homologs",.25f);
		summCols[2].setMinWidth(30);
		summCols[2].setMaxWidth(200);
		
		summCols[3] = new TableColumn("Comprehensive",.20f);
		summCols[3].setMinWidth(30);
		summCols[3].setMaxWidth(200);
		
	}
	
	public Table getView() {
		return summTbl;
	}

}
