package org.bbop.client.View;

import com.extjs.gxt.ui.client.Style;
import com.extjs.gxt.ui.client.widget.ContentPanel;
import com.extjs.gxt.ui.client.widget.layout.FillLayout;

import org.bbop.client.Listener.RefGenomeViewListenerI;

import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.VerticalPanel;

public class TargetInfoView {
	private RefGenomeViewListenerI refgListener;
	private RefGenomeView mainView;
	
	private ContentPanel infoPanel;
	private HorizontalPanel geneidBar ;
	private HorizontalPanel genesymBar ;
	private HorizontalPanel dateBar ;
	private HorizontalPanel pidBar;
	private HorizontalPanel omimBar;
	private HorizontalPanel curatorBar;
	private VerticalPanel targetInfo ;
	
	Label idLabel ;
	Label symLabel;
	Label pidLabel;
	Label omimLabel;
	Label curatorLabel;
	Label dateLabel;
	
	Label idValue;
	Label symValue;
	Label pidValue;
	Label omimValue;
	Label curatorValue;
	Label dateValue;
	
	public TargetInfoView (RefGenomeViewListenerI listener, RefGenomeView parent) {
		refgListener = listener;
		mainView = parent;
		
		infoPanel = new ContentPanel();
		geneidBar = new HorizontalPanel();
		genesymBar = new HorizontalPanel();
		dateBar = new HorizontalPanel();
		pidBar = new HorizontalPanel();
		omimBar = new HorizontalPanel();
		curatorBar = new HorizontalPanel();
		targetInfo = new VerticalPanel();
		
		idLabel = new Label("Gene id: ");
		symLabel = new Label("Gene symbol: ");
		pidLabel = new Label("Protein id: ");
		omimLabel = new Label("Omim id: ");
		curatorLabel = new Label("Curator: ");
		dateLabel = new Label("Completion date: ");
		
		idValue = new Label();
		symValue = new Label();
		pidValue = new Label();
		omimValue = new Label();
		curatorValue = new Label();
		dateValue = new Label();
		
		setAttr();
		addObserves();
	}
	
	public void setAttr() {
		idLabel.setStyleName("target-label");
		idValue.setStyleName("target-value");
		geneidBar.setWidth("40%");
		
		symLabel.setStyleName("target-label");
		symValue.setStyleName("target-value");
		genesymBar.setWidth("40%");
		
		pidLabel.setStyleName("target-label");
		pidValue.setStyleName("target-value");
		pidBar.setWidth("40%");
		
		omimLabel.setStyleName("target-label");
		omimValue.setStyleName("target-value");
		omimBar.setWidth("40%");
		
		dateLabel.setStyleName("target-label");
		dateValue.setStyleName("target-value");
		dateBar.setWidth("40%");
		
		curatorLabel.setStyleName("target-label");
		curatorValue.setStyleName("target-value");
		curatorBar.setWidth("40%");
		
		targetInfo.setSpacing(2);
		targetInfo.setStyleName("target-info");
		
		infoPanel.setHeading("Target gene information");
		
	}
	
	public void createView() {
		geneidBar.add(idLabel);
		geneidBar.add(idValue);
		
		genesymBar.add(symLabel);
		genesymBar.add(symValue);
		
		pidBar.add(pidLabel);
		pidBar.add(pidValue);
		
		omimBar.add(omimLabel);
		omimBar.add(omimValue);
		
		curatorBar.add(curatorLabel);
		curatorBar.add(curatorValue);

		dateBar.add(dateLabel);
		dateBar.add(dateValue);
		
		targetInfo.add(geneidBar);
		targetInfo.add(genesymBar);
		targetInfo.add(pidBar);
		targetInfo.add(omimBar);
		targetInfo.add(curatorBar);
		targetInfo.add(dateBar);
		
		infoPanel.setLayout(new FillLayout());
		infoPanel.add(targetInfo);
		
	}

	public void addObserves() {
		
	}
	
	public ContentPanel getView () { return infoPanel; }
	
	
	public void setGeneId (String id) {
		idValue.setText(id);
		
	}
	
	public void setGeneSymbol (String sym) {
		symValue.setText(sym);
		
	}
	
	public void setProteinId (String id) {
		pidValue.setText(id);
		
	}
	
	public void setOmimId (String id) {
		omimValue.setText(id);
		
	}
	
	public void setTargetDate(String date) {
		dateValue.setText(date);
		
	}
	
	public void setCurator(String name) {
		curatorValue.setText(name);
		
	}
	
	
}
