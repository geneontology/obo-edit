package org.bbop.client.View;

import net.mygwt.ui.client.Style;
import net.mygwt.ui.client.widget.Button;
import net.mygwt.ui.client.widget.ToolBar;
import net.mygwt.ui.client.widget.ToolItem;
import net.mygwt.ui.client.widget.ToolItemAdapter;

import org.bbop.client.Listener.RefGenomeViewListenerI;

public class TargetListToolBarView {
	private RefGenomeViewListenerI refgListener;
	private RefGenomeView mainView;
	private ToolBar tbar;
	
	private Button orthoBtn;
	private Button delTarget;
	
	private ToolItemAdapter orthoItem;
	private ToolItemAdapter delTargetItem;
	
	public TargetListToolBarView (RefGenomeViewListenerI listener, RefGenomeView parent) {
		refgListener = listener;
		mainView = parent;
		
		tbar = new ToolBar();
		
		orthoBtn = new Button("Ortholog list");
		delTarget = new Button("Delete target");
		
		orthoBtn.setIconStyle("icon-view");
    	delTarget.setIconStyle("icon-database-delete");
    	
    	orthoItem = new ToolItemAdapter(orthoBtn);
    	delTargetItem = new ToolItemAdapter(delTarget);
		
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
		tbar.add(new ToolItem(Style.SEPARATOR));
    	tbar.add(delTargetItem);
		
	}
	
	public ToolBar getView() {
		return tbar;
	}
	
}
