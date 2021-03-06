package org.bbop.client.View;

import net.mygwt.ui.client.Events;
import net.mygwt.ui.client.Style;
import net.mygwt.ui.client.event.BaseEvent;
import net.mygwt.ui.client.event.Listener;
import net.mygwt.ui.client.event.SelectionListener;
import net.mygwt.ui.client.widget.Button;
import net.mygwt.ui.client.widget.Dialog;
import net.mygwt.ui.client.widget.Info;
import net.mygwt.ui.client.widget.ToolBar;
import net.mygwt.ui.client.widget.ToolItem;
import net.mygwt.ui.client.widget.ToolItemAdapter;
import net.mygwt.ui.client.widget.WidgetContainer;
import net.mygwt.ui.client.widget.layout.FillLayout;

import org.bbop.client.Listener.RefGenomeViewListenerI;

import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.TextBox;
import com.google.gwt.user.client.ui.VerticalPanel;

public class TargetToolBarView {
	
	private RefGenomeViewListenerI refgListener;
	private RefGenomeView mainView;
	private ToolBar tbar;
	
	private Button summBtn;
//	private Button orthoBtn;
//	private Button reportBtn;
	private Button addTarget;
//	private Button delTarget;
	
	private ToolItemAdapter summItem;
//	private ToolItemAdapter orthoItem;
//	private ToolItemAdapter reportItem;
	private ToolItemAdapter addTargetItem;
//	private ToolItemAdapter delTargetItem;
	
	public TargetToolBarView(RefGenomeViewListenerI listener, RefGenomeView parent) {
		refgListener = listener;
		parent = mainView;
		
		tbar = new ToolBar();
		
		summBtn = new Button("Summary");
	//	orthoBtn = new Button("Ortholog");
	//	reportBtn = new Button("Curation report");
		addTarget = new Button("Add target");
	//	delTarget = new Button("Delete target");
		
		summBtn.setIconStyle("icon-application-view-list");
	//	orthoBtn.setIconStyle("icon-view");
	//	reportBtn.setIconStyle("icon-report");
		addTarget.setIconStyle("icon-database-add");
	//	delTarget.setIconStyle("icon-database-delete");
		
		summItem = new ToolItemAdapter(summBtn);
	//	orthoItem = new ToolItemAdapter(orthoBtn);
	//	reportItem = new ToolItemAdapter(reportBtn);
		addTargetItem = new ToolItemAdapter(addTarget);
	//	delTargetItem = new ToolItemAdapter(delTarget);
		
		setAttr();
		addObservers();
		
	}
	
	public void createView() {
		tbar.add(summItem);
		tbar.add(new ToolItem(Style.SEPARATOR));
	//	tbar.add(orthoItem);
	//	tbar.add(new ToolItem(Style.SEPARATOR));
	//	tbar.add(reportItem);
	//	tbar.add(new ToolItem(Style.SEPARATOR));
		tbar.add(addTargetItem);
	//	tbar.add(new ToolItem(Style.SEPARATOR));
	//	tbar.add(delTargetItem);
		
	}
	
	public void setAttr() {
		summItem.setStyleAttribute("paddingTop", "4px");
		summItem.setStyleAttribute("paddingLeft", "5px");
		summItem.setStyleAttribute("paddingBottom", "6px");
		summItem.setStyleAttribute("paddingRight", "8px");
		
//		orthoItem.setStyleAttribute("paddingTop", "4px");
//		orthoItem.setStyleAttribute("paddingRight", "5px");
//		orthoItem.setStyleAttribute("paddingBottom", "6px");
//		orthoItem.setStyleAttribute("paddingLeft", "8px");
		
//		reportItem.setStyleAttribute("paddingTop", "4px");
//		reportItem.setStyleAttribute("paddingRight", "5px");
//		reportItem.setStyleAttribute("paddingBottom", "6px");
//		reportItem.setStyleAttribute("paddingLeft", "8px");
		
		addTargetItem.setStyleAttribute("paddingTop", "4px");
		addTargetItem.setStyleAttribute("paddingRigth", "5px");
		addTargetItem.setStyleAttribute("paddingBottom", "6px");
		addTargetItem.setStyleAttribute("paddingLeft", "8px");
		
//		delTargetItem.setStyleAttribute("paddingTop", "4px");
//		delTargetItem.setStyleAttribute("paddingBottom", "6px");
//		delTargetItem.setStyleAttribute("paddingLeft", "8px");
	}
	
	public void addObservers() {
		addTarget.addSelectionListener(new AddTargetListener());
		
	}
	
	public ToolBar getView() {
		return tbar;
	}
	
	private class AddTargetListener implements SelectionListener {

		public void widgetSelected(BaseEvent be) {
			// TODO Auto-generated method stub
			final HorizontalPanel geneidBar = new HorizontalPanel();
			final HorizontalPanel genesymBar = new HorizontalPanel();
			final HorizontalPanel dateBar = new HorizontalPanel();
			final VerticalPanel targetDialog = new VerticalPanel();
			
			Label idLabel = new Label("Gene id: ");
			Label symLabel = new Label("Gene symbol: ");
			Label dateLabel = new Label("Target Completion date ");
			
			TextBox idTerm = new TextBox();
			TextBox symTerm = new TextBox();
			TextBox dateTerm = new TextBox();
			
			geneidBar.setSpacing(10);
			genesymBar.setSpacing(10);
			dateBar.setSpacing(10);
			targetDialog.setSpacing(8);
			
			idTerm.setWidth("90");
			symTerm.setWidth("90");
			dateTerm.setWidth("90");
			
			geneidBar.add(idLabel);
			geneidBar.add(idTerm);
			genesymBar.add(symLabel);
			genesymBar.add(symTerm);
			dateBar.add(dateLabel);
			dateBar.add(dateTerm);
			
			targetDialog.add(geneidBar);
			targetDialog.add(genesymBar);
			targetDialog.add(dateBar);
			
			final Dialog complex = new Dialog(Style.OK_CANCEL | Style.CLOSE | Style.RESIZE);  
			complex.setMinimumSize(300, 200);  
			complex.addStyleName("my-shell-plain"); 
			complex.setText("Add new target gene");
			complex.setCloseOnButtonClick(true);
			
			WidgetContainer c = complex.getContent();
			c.setBorders(false);
			c.setLayout(new FillLayout());
			c.add(targetDialog);
			complex.open();
			
			
			complex.addListener(Events.Close, new Listener() {

				public void handleEvent(BaseEvent be) {
					// TODO Auto-generated method stub
					Button btn = complex.getButtonPressed();
					if (btn.getButtonId() == Dialog.OK_ID) {
						Info.show("Adding new target", "Name: ", "Gene");
						
					}
					 complex.close();
						
				}
					
			
				
			});
		
	  }
	
	
	}

}
