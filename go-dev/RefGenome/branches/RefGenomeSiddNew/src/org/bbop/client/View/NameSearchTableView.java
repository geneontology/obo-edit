package org.bbop.client.View;

import net.mygwt.ui.client.Style;
import net.mygwt.ui.client.widget.table.Table;
import net.mygwt.ui.client.widget.table.TableColumn;
import net.mygwt.ui.client.widget.table.TableColumnModel;
import net.mygwt.ui.client.widget.table.TableItem;

import org.bbop.client.Listener.RefGenomeViewListenerI;
import org.bbop.client.model.NodeDTO;

public class NameSearchTableView {
    RefGenomeViewListenerI refgListener;
    RefGenomeView mainView;
	
    private TableColumnModel nameColModel;
	private Table nameTbl;
	private TableColumn[] nameCols;
    
    
	public NameSearchTableView (RefGenomeViewListenerI listener, RefGenomeView parent) {
		refgListener = listener;
		mainView = parent;
	}
	
	public void addObservers () {
		
	}
	
	public void createView(NodeDTO[] result) {
		nameCols = new TableColumn[3];
		
		nameCols[0] = new TableColumn("Label",.50f);
		nameCols[0].setMinWidth(30);
		nameCols[0].setMaxWidth(300);
		
		nameCols[1] = new TableColumn("Id",.25f);
		nameCols[1].setMinWidth(30);
		nameCols[1].setMaxWidth(300);
		
		nameCols[2] = new TableColumn("Source Id",.25f);
		nameCols[2].setMinWidth(30);
		nameCols[2].setMaxWidth(300);
		
		nameColModel = new TableColumnModel(nameCols);
		nameTbl = new Table(Style.MULTI, nameColModel);
		nameTbl.setBorders(true);
		
		for(int i = 0 ; i < result.length; i++) {
			String[] nodeData = new String[3];
			nodeData[i] = result[i].getLabel();
			nodeData[i] = result[i].getId();
			nodeData[i] = result[i].getSourceId();
			nameTbl.add(new TableItem(nodeData));
		}
		
	}
	
	public Table getView() { return nameTbl; }
}
