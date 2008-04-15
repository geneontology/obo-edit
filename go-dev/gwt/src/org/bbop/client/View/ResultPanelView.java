package org.bbop.client.View;

import org.bbop.client.Listener.RefGenomeViewListenerI;

import net.mygwt.ui.client.Style;
import net.mygwt.ui.client.widget.ContentPanel;
import net.mygwt.ui.client.widget.TabFolder;
import net.mygwt.ui.client.widget.TabItem;
import net.mygwt.ui.client.widget.WidgetContainer;
import net.mygwt.ui.client.widget.layout.FillLayout;
import net.mygwt.ui.client.widget.table.Table;


public class ResultPanelView {
	private RefGenomeViewListenerI refgListener;
	private RefGenomeView mainView;
	
	
	private ContentPanel centerPanel;
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
	
	public void addTableView(Table tbl) {
		WidgetContainer tblHolder = new WidgetContainer();
		TabFolder tblFolder = new TabFolder(Style.TOP);
		tblFolder.setTabWidth(80);
		
		TabItem tblItem = new TabItem(Style.NONE);
		tblItem.setText("Search result");
		tblItem.setIconStyle("icon-tabs");
		WidgetContainer itemContainer = tblItem.getContainer();
		itemContainer.setLayout(new FillLayout());
		itemContainer.add(tbl);
		tblFolder.add(tblItem);
		tblFolder.setSelection(tblItem);
		
		tblHolder.setLayout(new FillLayout(15));
		tblHolder.add(tblFolder);
		centerPanel.add(tblHolder);
		centerPanel.layout(true);
		
	}
	
	
	
	
}
