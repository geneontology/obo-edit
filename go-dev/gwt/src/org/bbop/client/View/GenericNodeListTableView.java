package org.bbop.client.View;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import net.mygwt.ui.client.Style;
import net.mygwt.ui.client.widget.Button;
import net.mygwt.ui.client.widget.table.DateTimeCellRenderer;
import net.mygwt.ui.client.widget.table.Table;
import net.mygwt.ui.client.widget.table.TableColumn;
import net.mygwt.ui.client.widget.table.TableColumnModel;
import net.mygwt.ui.client.widget.table.TableItem;

import org.bbop.client.Listener.RefGenomeViewListenerI;
import org.bbop.client.model.NodeDTO;


import com.google.gwt.user.client.ui.CheckBox;

/**
 * highly configurable table display for showing lists of nodes
 * @author cjm
 *
 */
public class GenericNodeListTableView {

	private RefGenomeViewListenerI refgListener;
	private RefGenomeView mainView;
	
	//private WidgetContainer summaryWidget;
	private TableColumnModel colModel;
	private Table targetTbl;
	private TableColumn[] tableCols;
	private Map columnLabelMap = new HashMap();
	
	/**
	 * @gwt.typeArgs <java.langString>
	 */
	private List columnHeadings = new ArrayList();
	
	public GenericNodeListTableView (RefGenomeViewListenerI listener, RefGenomeView parent) {
		refgListener = listener;
		mainView = parent;
		addColumnHeading("id");
		addColumnHeading("label");
	}
	
	public List getColumnHeadings() {
		return columnHeadings;
	}
	public void addColumnHeading(String c) {
		 columnHeadings.add(c);
	}
	public void addColumnHeading(String c, String label) {
		addColumnHeading(c);
		columnLabelMap.put(c, label);
	}


	public void setColumnHeadings(List columnHeadings) {
		this.columnHeadings = columnHeadings;
	}

	public int getNumberOfColumnHeadings() {
		return getColumnHeadings().size();
	}

	public void createView(NodeDTO[] nodes) {

		// This is where it should receive data from server
		tableCols = new TableColumn[getNumberOfColumnHeadings()];

		List cols = getColumnHeadings();

		for (int i = 0; i < cols.size(); i++) {

			String col = (String) cols.get(i);
			String colLabel = (String) (columnLabelMap.containsKey(col) ? columnLabelMap.get(col) : col);
			tableCols[i] = new TableColumn(colLabel,.15f);
			tableCols[i].setMinWidth(20);
			tableCols[i].setMaxWidth(100);
			System.err.println("set up col: "+i);
		}


		colModel = new TableColumnModel(tableCols);
		targetTbl = new Table(Style.MULTI,colModel);
		targetTbl.setBorders(true);

		for(int i = 0; i < nodes.length; i++) {
			Object[] targetData = new Object[cols.size()];
			NodeDTO n = (NodeDTO) nodes[i];
			System.err.println("adding to table: "+n);
			for (int c=0; c < cols.size(); c++) {
				String cn = (String) cols.get(c);
				String val = "-";
				if (cn.equals("id")) {
					val = n.getId();
				}
				else if (cn.equals("label")) {
					val = n.getLabel();
				}
				else {
					List vals = n.getTargetIds(cn);
					StringBuffer sb = new StringBuffer();
					Iterator it = vals.iterator();
					while (it.hasNext()) {
						sb.append(it.next().toString());
						if (it.hasNext())
							sb.append(", ");
					}
					val = sb.toString();
				}
				targetData[c] = val;
			}
 
			targetTbl.add(new TableItem(targetData));

		}



	}
	
	public void setAttr() {
		
	}
	
	public void addObservers () {
		
	}
	
	public Table getView() {
		return targetTbl;
	}



	
	
}
