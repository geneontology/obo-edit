package org.bbop.client.View;

import org.bbop.client.Listener.RefGenomeViewListenerI;

import com.extjs.gxt.ui.client.Style;
import com.extjs.gxt.ui.client.Style.Orientation;
import com.extjs.gxt.ui.client.widget.ContentPanel;
import com.extjs.gxt.ui.client.widget.PagingToolBar;
import com.extjs.gxt.ui.client.widget.TabPanel;
import com.extjs.gxt.ui.client.widget.TabItem;
import com.extjs.gxt.ui.client.widget.toolbar.ToolBar;
import com.extjs.gxt.ui.client.widget.LayoutContainer;
import com.extjs.gxt.ui.client.widget.layout.FillLayout;
import com.extjs.gxt.ui.client.widget.layout.FitLayout;
import com.extjs.gxt.ui.client.widget.layout.RowData;
import com.extjs.gxt.ui.client.widget.layout.RowLayout;
import com.extjs.gxt.ui.client.widget.table.Table;
import com.extjs.gxt.ui.client.widget.table.TableItem;


public class ResultPanelView {
	private RefGenomeViewListenerI refgListener;
	private RefGenomeView mainView;
	
	
	private ContentPanel centerPanel;
	private LayoutContainer tblHolder;
	private boolean bootStrap = true;
	
	
	private SummaryTableView summView;
	
	public ResultPanelView(RefGenomeViewListenerI listener,RefGenomeView parent){
		refgListener = listener;
		mainView = parent;
		centerPanel = new ContentPanel();
		centerPanel.setLayout(new FitLayout());
		
	}

	public void createView() {
		centerPanel.setHeading("Display panel");
		if(bootStrap) {
			summView = new SummaryTableView(refgListener,mainView);
			summView.createView();
			LayoutContainer summTblHolder = new LayoutContainer();
			ContentPanel summTblPanel = new ContentPanel();
			summTblPanel.setCollapsible(true);
			summTblPanel.setAnimCollapse(false);
			summTblPanel.setHeading("Summary result");
			summTblPanel.setLayout(new FillLayout());
			summTblPanel.add(summView.getView());
			
			summTblHolder.setLayout(new FillLayout());
			summTblHolder.add(summTblPanel);
			centerPanel.add(summTblHolder);
		}
		//centerPanel.layout(true);
			
		
    }
	
	public void setBootStrap(boolean flag) {
		bootStrap = flag;
	}
	
	public ContentPanel getView(){
		return centerPanel;
	}
	
	public void removeChildViews() {
		centerPanel.removeAll();
	}
	
	public void addTableView(Table tbl,String title) {
		tblHolder = new LayoutContainer();
		TabPanel tblFolder = new TabPanel();
		tblFolder.setTabWidth(80);
		
		TabItem tblItem = new TabItem();
		tblItem.setText(title);
		tblItem.setIconStyle("icon-tabs");
		tblItem.setLayout(new FillLayout());
		tblItem.add(tbl);
		tblFolder.add(tblItem);
		tblFolder.setSelection(tblItem);
		
		tblHolder.setLayout(new FillLayout());
		tblHolder.add(tblFolder);
		
		
	}
	
	public void addTableToolBarView(Table tbl, ToolBar tbar, String title) {
		tblHolder = new LayoutContainer();
		TabPanel tblFolder = new TabPanel();
		tblFolder.setTabWidth(80);
		
		TabItem tblItem = new TabItem();
		tblItem.setText(title);
		tblItem.setIconStyle("icon-tabs");
		tblItem.setLayout(new FillLayout());
		tblItem.add(tbl);
		tblFolder.add(tblItem);
		tblFolder.setSelection(tblItem);
		  
		RowLayout layout = new RowLayout(Orientation.VERTICAL);   
		tblHolder.setLayout(layout); 
		
		tblHolder.add(tbar,new RowData(1,-1));
		tblHolder.add(tblFolder, new RowData(1,1));
		
	}
	
	public void addTargetListView(Table tbl, ToolBar[] tbars, String title) {
		tblHolder = new LayoutContainer();
		TabPanel tblFolder = new TabPanel();
		tblFolder.setTabWidth(80);
		
		TabItem tblItem = new TabItem();
		tblItem.setText(title);
		tblItem.setIconStyle("icon-tabs");
		tblItem.setLayout(new RowLayout(Orientation.VERTICAL));
		
		//adds the toolbar for target list manipulation
		tblItem.add(tbars[1],new RowData(1,-1));
		tblItem.add(tbl,new RowData(1,1));
		tblFolder.add(tblItem);
		tblFolder.setSelection(tblItem);
		
		//tblHolder.setScrollEnabled(false);  
		RowLayout layout = new RowLayout(Orientation.VERTICAL);  
		//layout.setMargin(8);  
		tblHolder.setLayout(layout); 
		
		//Add toolbar and tabview
		tblHolder.add(tbars[0],new RowData(1,-1));
		tblHolder.add(tblFolder, new RowData(1,1));
		
		//Now add the target information view
		TargetInfoView infoView = new TargetInfoView(refgListener, mainView);
		TableItem targetInfo = tbl.getItem(0);
		infoView.setGeneSymbol((String) targetInfo.getValue(0));
		infoView.setGeneId(targetInfo.getValue(1).toString());
		infoView.setProteinId(targetInfo.getValue(2).toString());
		infoView.setTargetDate(targetInfo.getValue(3).toString());
		infoView.setOmimId("omim value");
		infoView.setCurator("Best curator");
		
		infoView.createView();
		tblHolder.add(infoView.getView(),new RowData(1,1));
		
	}
	
	public void addOrthologListView(ContentPanel orthoPanel, String title) {
		tblHolder = new LayoutContainer();
		TabPanel tblFolder = new TabPanel();
		tblFolder.setTabWidth(80);
		
		TabItem tblItem = new TabItem();
		tblItem.setText(title);
		tblItem.setIconStyle("icon-tabs");
		tblItem.setLayout(new RowLayout(Orientation.VERTICAL));
		
	
		
		tblItem.add(orthoPanel,new RowData(1,1));
		tblFolder.add(tblItem);
		tblFolder.setSelection(tblItem);
		
		//tblHolder.setScrollEnabled(false);
		tblHolder.setLayout(new FillLayout());
		tblHolder.add(tblFolder);
		
	}
	

	
	public void resetView() {
		centerPanel.add(tblHolder);
		centerPanel.layout();
	}
	
	public void attachView() {
		centerPanel.add(tblHolder);
	}
	
	
	
	
}
