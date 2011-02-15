package org.bbop.client.View;



import org.bbop.client.Listener.RefGenomeViewListenerI;
import com.extjs.gxt.ui.client.widget.table.Table;
import com.extjs.gxt.ui.client.widget.table.TableColumn;
import com.extjs.gxt.ui.client.widget.table.TableColumnModel;
import com.extjs.gxt.ui.client.widget.table.TableItem;

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
		
		summTbl = new Table(summColModel);
		summTbl.setBorders(false);
		summTbl.add(new TableItem(new String[]{"Fly","185","19","160","86%"}));
		summTbl.add(new TableItem(new String[]{"Worm","185","19","160","86%"}));
		summTbl.add(new TableItem(new String[]{"Fission yeast","185","19","160","86%"}));
		summTbl.add(new TableItem(new String[]{"Budding yeast","185","19","160","86%"}));
		summTbl.add(new TableItem(new String[]{"Mouse","185","19","160","86%"}));
		summTbl.add(new TableItem(new String[]{"Plant","185","19","160","86%"}));
		summTbl.add(new TableItem(new String[]{"Zebrafish","185","19","160","86%"}));
		summTbl.add(new TableItem(new String[]{"Social ameoba","185","19","160","86%"}));
		summTbl.add(new TableItem(new String[]{"E.coli","185","19","160","86%"}));	
		summTbl.add(new TableItem(new String[]{"Rat","185","19","160","86%"}));
		
		
		
			
		//summaryWidget.setLayout(new FillLayout(10));
		//summaryWidget.add(summTbl);
	}
	
	public void addObservers() {
		
	}
	
	public void setAttr() {
		
		
		
		summCols = new TableColumn[5];
		
		summCols[0] = new TableColumn("Species",.20f);
		summCols[0].setMinWidth(30);
		summCols[0].setMaxWidth(200);
		
		summCols[1] = new TableColumn("Homologs",.20f);
		summCols[1].setMinWidth(30);
		summCols[1].setMaxWidth(200);
		
		summCols[2] = new TableColumn("No Homologs",.20f);
		summCols[2].setMinWidth(30);
		summCols[2].setMaxWidth(200);
		
		summCols[3] = new TableColumn("Comprehensive",.20f);
		summCols[3].setMinWidth(30);
		summCols[3].setMaxWidth(200);
		
		summCols[4] = new TableColumn("Completed",.20f);
		summCols[4].setMinWidth(30);
		summCols[4].setMaxWidth(200);
		
	}
	
	public Table getView() {
		return summTbl;
	}

}
