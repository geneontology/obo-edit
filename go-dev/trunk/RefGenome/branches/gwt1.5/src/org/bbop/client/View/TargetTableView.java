package org.bbop.client.View;

import java.util.ArrayList;
import java.util.List;


import com.extjs.gxt.ui.client.store.ListStore;

import com.extjs.gxt.ui.client.widget.grid.ColumnConfig;
import com.extjs.gxt.ui.client.widget.grid.ColumnModel;
import com.extjs.gxt.ui.client.widget.grid.Grid;


import org.bbop.client.Listener.RefGenomeViewListenerI;
import org.bbop.client.model.TestData;
import org.bbop.client.model.samples.TargetList;


import com.google.gwt.i18n.client.DateTimeFormat;


public class TargetTableView {

	private RefGenomeViewListenerI refgListener;
	private RefGenomeView mainView;
	
	List<ColumnConfig> configs = new ArrayList<ColumnConfig>(); 
	Grid<TargetList> grid;
	
	
	public TargetTableView (RefGenomeViewListenerI listener, RefGenomeView parent) {
		refgListener = listener;
		mainView = parent;
		
		
	}
	
	public void createView() {
		
		ColumnConfig column = new ColumnConfig();
		
	
		column.setId("name");
		column.setHeader("Gene Symbol");
		column.setWidth(100);
		configs.add(column);
		
		column = new ColumnConfig();
		column.setId("id");
		column.setHeader("Gene Id");
		column.setWidth(100);
		configs.add(column);
		
		column = new ColumnConfig();
		column.setId("uniprot");
		column.setHeader("Protein Id");
		column.setWidth(100);
		configs.add(column);
		
		
		column = new ColumnConfig("completed","Completion date", 100);
		column.setDateTimeFormat(DateTimeFormat.getShortDateFormat());
		configs.add(column);
		
		
		//Now the data store
		ListStore<TargetList> store = new ListStore<TargetList>();
		store.add(TestData.getTarget());
		
		
		ColumnModel targetColModel = new ColumnModel(configs);
	    grid = new Grid<TargetList>(store,targetColModel);
	    grid.setBorders(true);
		
		
		
	}
	
	public void setAttr() {
		
	}
	
	public void addObservers () {
		
	}
	
	public Grid getView() {
		return grid;
	}
	
}
