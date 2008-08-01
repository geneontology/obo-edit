package org.bbop.client.View;

import com.extjs.gxt.ui.client.Style;
import com.extjs.gxt.ui.client.widget.button.Button;
import com.extjs.gxt.ui.client.widget.toolbar.SeparatorToolItem;
import com.extjs.gxt.ui.client.widget.toolbar.ToolBar;
import com.extjs.gxt.ui.client.widget.toolbar.ToolItem;
import com.extjs.gxt.ui.client.widget.toolbar.AdapterToolItem;

import org.bbop.client.Listener.RefGenomeViewListenerI;

public class TargetListToolBarView {
	private RefGenomeViewListenerI refgListener;
	private RefGenomeView mainView;
	private ToolBar tbar;
	
	private Button orthoBtn;
	private Button delTarget;
	
	private AdapterToolItem orthoItem;
	private AdapterToolItem delTargetItem;
	
	public TargetListToolBarView (RefGenomeViewListenerI listener, RefGenomeView parent) {
		refgListener = listener;
		mainView = parent;
		
		tbar = new ToolBar();
		
		orthoBtn = new Button("Ortholog list");
		delTarget = new Button("Delete target");
		
		orthoBtn.setIconStyle("icon-view");
    	delTarget.setIconStyle("icon-database-delete");
    	
    	orthoItem = new AdapterToolItem(orthoBtn);
    	delTargetItem = new AdapterToolItem(delTarget);
		
		setAttr();
		addObservers();
		
	}
	
	public void setAttr() {
		orthoItem.setStyleAttribute("paddingTop", "4px");
		orthoItem.setStyleAttribute("paddingRight", "5px");
		orthoItem.setStyleAttribute("paddingBottom", "6px");
		orthoItem.setStyleAttribute("paddingLeft", "5px");
	
		delTargetItem.setStyleAttribute("paddingTop", "4px");
		delTargetItem.setStyleAttribute("paddingBottom", "6px");
		delTargetItem.setStyleAttribute("paddingLeft", "5px");
		
		
	}
	
	public void addObservers () {
		
	}
	
	public void createView () {
		tbar.add(orthoItem);
		tbar.add(new SeparatorToolItem());
    	tbar.add(delTargetItem);
		
	}
	
	public ToolBar getView() {
		return tbar;
	}
	
}
