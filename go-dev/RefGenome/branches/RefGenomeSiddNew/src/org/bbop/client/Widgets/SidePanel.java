package org.bbop.client.Widgets;

import java.util.HashMap;

import net.mygwt.ui.client.Style;
import net.mygwt.ui.client.widget.ContentPanel;
import net.mygwt.ui.client.widget.ExpandBar;
import net.mygwt.ui.client.widget.ExpandItem;
import net.mygwt.ui.client.widget.WidgetContainer;
import net.mygwt.ui.client.widget.layout.FillLayout;

public class SidePanel extends RefGWidgetTemplate{
	
	private ContentPanel west ;
	private ExpandBar exbar;
	private HashMap baritems = new HashMap();
	
	public SidePanel () {
		west = new ContentPanel(Style.HEADER);
		west.setLayout(new FillLayout());
		west.setText("Tools");
		west.setStyleName("title");
	}

	/* (non-Javadoc)
	 * @see org.bbop.client.Widgets.RefGWidgetTemplate#configureComponents()
	 */
	public void configureComponents() {
		// TODO Auto-generated method stub
		exbar.setBorders(true);
		
	}

	/* (non-Javadoc)
	 * @see org.bbop.client.Widgets.RefGWidgetTemplate#configureItems()
	 */
	public void configureItems() {
		// TODO Auto-generated method stub
		ExpandItem browseitem = (ExpandItem) baritems.get("browseitems");
		ExpandItem searchitem = (ExpandItem) baritems.get("searchitems");
		
		browseitem.setText("Browse");
		browseitem.getContainer().addText("List of genes");
		
		searchitem.setText("Search");
		searchitem.getContainer().addText("Search genes");
	}

	/* (non-Javadoc)
	 * @see org.bbop.client.Widgets.RefGWidgetTemplate#configureSubComponents()
	 */
	public void configureSubComponents() {
		// TODO Auto-generated method stub
		
		
		
	}

	/* (non-Javadoc)
	 * @see org.bbop.client.Widgets.RefGWidgetTemplate#makeWidget()
	 */
	public void makeWidget() {
		// TODO Auto-generated method stub
		exbar.add((ExpandItem) baritems.get("browseitems"));
		exbar.add((ExpandItem) baritems.get("searchitems"));
		west.add(exbar);
	}

	/* (non-Javadoc)
	 * @see org.bbop.client.Widgets.RefGWidgetTemplate#setComponents()
	 */
	public void setComponents() {
		// TODO Auto-generated method stub
		exbar = new ExpandBar(Style.MULTI);
	}

	/* (non-Javadoc)
	 * @see org.bbop.client.Widgets.RefGWidgetTemplate#setItems()
	 */
	public void setItems() {
		// TODO Auto-generated method stub
		baritems.put("browseitems", new ExpandItem());
		baritems.put("searchitems", new ExpandItem());
		
	}

	/* (non-Javadoc)
	 * @see org.bbop.client.Widgets.RefGWidgetTemplate#setSubComponents()
	 */
	public void setSubComponents() {
		// TODO Auto-generated method stub
		
	}
	
	public boolean isSubComponents() {
		return false;
	}
	
	public boolean isItems() {
		return true;
	}
	
	public WidgetContainer getWidget() {
		return west;
	}

}
