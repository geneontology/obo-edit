package org.bbop.client.View;



import net.mygwt.ui.client.Style;
import net.mygwt.ui.client.widget.ContentPanel;
import net.mygwt.ui.client.widget.MessageBox;
import net.mygwt.ui.client.widget.TabFolder;
import net.mygwt.ui.client.widget.TabItem;
import net.mygwt.ui.client.widget.WidgetContainer;
import net.mygwt.ui.client.widget.layout.FillLayout;
import net.mygwt.ui.client.widget.table.CellRenderer;
import net.mygwt.ui.client.widget.table.Table;
import net.mygwt.ui.client.widget.table.TableColumn;
import net.mygwt.ui.client.widget.table.TableColumnModel;
import net.mygwt.ui.client.widget.table.TableItem;

import org.bbop.client.Listener.RefGenomeViewListenerI;
import org.bbop.client.model.NodeDTO;

import com.google.gwt.http.client.URL;
import com.google.gwt.user.client.ui.ClickListener;
import com.google.gwt.user.client.ui.HTML;
import com.google.gwt.user.client.ui.Widget;

public class NameSearchTableView {
    RefGenomeViewListenerI refgListener;
    RefGenomeView mainView;
	
    private TableColumnModel nameColModel;
	private Table nameTbl;
	private TableColumn[] nameCols;
	
	final String entrezUrl = "http://view.ncbi.nlm.nih.gov/gene/"; 
	final String omimUrl = "http://www.ncbi.nlm.nih.gov/entrez/dispomim.cgi?id=";
	
 
    
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
	//	nameCols[1].setRenderer(new CellRenderer() {
	//	public String render(String property, Object value) {
	//			// TODO Auto-generated method stub
	//			String id = ((HTML) value).getHTML();
	//			String[] token = id.split(":");
	//			String entrezLink = "<a href='http://www.ncbi.nlm.nih.gov/sites/entrez?db=gene&cmd=Retrieve&dopt=full_report&list_uids=" + token[1] + "'>" + id + "</a>";
	//			return entrezLink;
	//	}
			
	//	});
		
		nameCols[2] = new TableColumn("Source Id",.25f);
		nameCols[2].setMinWidth(30);
		nameCols[2].setMaxWidth(300);
		
		nameColModel = new TableColumnModel(nameCols);
		nameTbl = new Table(Style.MULTI, nameColModel);
		nameTbl.setBorders(true);
		
		
		for(int i = 0 ; i < result.length; i++) {
			Object[] nodeData = new Object[3];
			nodeData[0] = result[i].getLabel();
			HTML linkedId = new HTML(result[i].getId());
			linkedId.addClickListener(new LinkedIdListener());
			nodeData[1] = linkedId;
			nodeData[2] = result[i].getSourceId();
			nameTbl.add(new TableItem(nodeData));
			
		}
		
	}
	
	
	public Table getView() { return nameTbl; }
	
	public TabFolder getTblContainer() {
		ContentPanel resultPanel = mainView.getResultPanel().getView();
		WidgetContainer container = (WidgetContainer) resultPanel.getWidget(0);
		TabFolder tblContainer = (TabFolder) container.getWidget(0);
		return tblContainer;
	}
	
	private class LinkedIdListener implements ClickListener {

		public void onClick(Widget sender) {
			// TODO Auto-generated method stub
		    String value = ((HTML) sender).getHTML();
		    String[] token = value.split(":");
		    String url;
		    
		    if (token[0].startsWith("OMIM")) {
		    	url = URL.encode(omimUrl + token[1]);
		    	
		    }
		    else {
		    	url = URL.encode(entrezUrl + token[1]);
		    }
				
			TabItem tblItem = new TabItem(Style.CLOSE);
			tblItem.setText(value);
			tblItem.setIconStyle("icon-tabs");
			tblItem.setURL(url);
			
			TabFolder tblContainer = getTblContainer();
			tblContainer.add(tblItem);
			tblContainer.setSelection(tblItem);
		}
		
	}
}
