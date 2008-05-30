package org.bbop.client.View;

import org.bbop.client.Listener.RefGenomeViewListenerI;

import net.mygwt.ui.client.Style;
import net.mygwt.ui.client.viewer.RemoteContentProvider;
import net.mygwt.ui.client.widget.ContentPanel;
import net.mygwt.ui.client.widget.PagingToolBar;
import net.mygwt.ui.client.widget.TabFolder;
import net.mygwt.ui.client.widget.TabItem;
import net.mygwt.ui.client.widget.ToolBar;
import net.mygwt.ui.client.widget.WidgetContainer;
import net.mygwt.ui.client.widget.layout.FillLayout;
import net.mygwt.ui.client.widget.layout.RowData;
import net.mygwt.ui.client.widget.layout.RowLayout;
import net.mygwt.ui.client.widget.table.Table;
import net.mygwt.ui.client.widget.table.TableItem;


public class ResultPanelView {
	private RefGenomeViewListenerI refgListener;
	private RefGenomeView mainView;
	
	
	private ContentPanel centerPanel;
	private WidgetContainer tblHolder;
	private boolean bootStrap = true;
	
	
	private SummaryTableView summView;
	
	public ResultPanelView(RefGenomeViewListenerI listener,RefGenomeView parent){
		refgListener = listener;
		mainView = parent;
		centerPanel = new ContentPanel(Style.HEADER);
		centerPanel.setLayout(new FillLayout());
		
	}

	public void createView() {
		centerPanel.setText("Display panel");
		if(bootStrap) {
			summView = new SummaryTableView(refgListener,mainView);
			summView.createView();
			WidgetContainer summTblHolder = new WidgetContainer();
			ContentPanel summTblPanel = new ContentPanel(Style.HEADER | Style.COLLAPSE);
			summTblPanel.setAnimateCollapse(false);
			summTblPanel.setText("Summary result");
			summTblPanel.setLayout(new FillLayout());
			summTblPanel.add(summView.getView());
			
			summTblHolder.setLayout(new FillLayout(15));
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
		tblHolder = new WidgetContainer();
		TabFolder tblFolder = new TabFolder(Style.TOP);
		tblFolder.setTabWidth(80);
		
		TabItem tblItem = new TabItem(Style.NONE);
		tblItem.setText(title);
		tblItem.setIconStyle("icon-tabs");
		WidgetContainer itemContainer = tblItem.getContainer();
		itemContainer.setLayout(new FillLayout());
		itemContainer.add(tbl);
		tblFolder.add(tblItem);
		tblFolder.setSelection(tblItem);
		
		tblHolder.setLayout(new FillLayout(15));
		tblHolder.add(tblFolder);
		
		
	}
	
	public void addTableToolBarView(Table tbl, ToolBar tbar, String title) {
		tblHolder = new WidgetContainer();
		TabFolder tblFolder = new TabFolder(Style.TOP);
		tblFolder.setTabWidth(80);
		
		TabItem tblItem = new TabItem(Style.NONE);
		tblItem.setText(title);
		tblItem.setIconStyle("icon-tabs");
		WidgetContainer itemContainer = tblItem.getContainer();
		itemContainer.setLayout(new FillLayout());
		itemContainer.add(tbl);
		tblFolder.add(tblItem);
		tblFolder.setSelection(tblItem);
		
		tblHolder.setScrollEnabled(false);  
		RowLayout layout = new RowLayout(Style.VERTICAL);  
		layout.setMargin(8);  
		tblHolder.setLayout(layout); 
		
		tblHolder.add(tbar,new RowData(RowData.FILL_HORIZONTAL));
		tblHolder.add(tblFolder, new RowData(RowData.FILL_BOTH));
		
	}
	
	public void addTargetListView(Table tbl, ToolBar[] tbars, String title) {
		tblHolder = new WidgetContainer();
		TabFolder tblFolder = new TabFolder(Style.TOP);
		tblFolder.setTabWidth(80);
		
		TabItem tblItem = new TabItem(Style.NONE);
		tblItem.setText(title);
		tblItem.setIconStyle("icon-tabs");
		WidgetContainer itemContainer = tblItem.getContainer();
		itemContainer.setLayout(new RowLayout(Style.VERTICAL));
		
		//adds the toolbar for target list manipulation
		itemContainer.add(tbars[1],new RowData(RowData.FILL_HORIZONTAL));
		itemContainer.add(tbl,new RowData(RowData.FILL_BOTH));
		tblFolder.add(tblItem);
		tblFolder.setSelection(tblItem);
		
		tblHolder.setScrollEnabled(false);  
		RowLayout layout = new RowLayout(Style.VERTICAL);  
		layout.setMargin(8);  
		tblHolder.setLayout(layout); 
		
		//Add toolbar and tabview
		tblHolder.add(tbars[0],new RowData(RowData.FILL_HORIZONTAL));
		tblHolder.add(tblFolder, new RowData(RowData.FILL_BOTH));
		
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
		tblHolder.add(infoView.getView(),new RowData(RowData.FILL_BOTH));
		
	}
	
	public void addOrthologListView(Table tbl, RemoteContentProvider cp, int items, String title) {
		tblHolder = new WidgetContainer();
		TabFolder tblFolder = new TabFolder(Style.TOP);
		tblFolder.setTabWidth(80);
		
		TabItem tblItem = new TabItem(Style.NONE);
		tblItem.setText(title);
		tblItem.setIconStyle("icon-tabs");
		WidgetContainer itemContainer = tblItem.getContainer();
		itemContainer.setLayout(new RowLayout(Style.VERTICAL));
		
		PagingToolBar pbar = new PagingToolBar(items);
		pbar.bind(cp);
		
		itemContainer.add(tbl,new RowData(RowData.FILL_BOTH));
		itemContainer.add(pbar, new RowData(RowData.FILL_HORIZONTAL));
		tblFolder.add(tblItem);
		tblFolder.setSelection(tblItem);
		
		tblHolder.setScrollEnabled(false);
		tblHolder.setLayout(new FillLayout());
		tblHolder.add(tblFolder);
		
	
		//cp.load(0,10);
		
	}
	

	
	public void resetView() {
		centerPanel.add(tblHolder);
		centerPanel.layout(true);
	}
	
	public void attachView() {
		centerPanel.add(tblHolder);
	}
	
	
	
	
}
