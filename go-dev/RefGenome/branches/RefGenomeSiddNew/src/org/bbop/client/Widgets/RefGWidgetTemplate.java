/**
 * 
 */
package org.bbop.client.Widgets;

/**
 * @author sid
 * 
 */

import org.bbop.client.Widgets.RefGWidgetInterface;

public abstract class RefGWidgetTemplate implements RefGWidgetInterface {
	
	public RefGWidgetTemplate(){}




	/* (non-Javadoc)
	 * @see org.bbop.client.Widgets.RefGWidgetInterface#configureComponents()
	 */
	public abstract void configureComponents();




	/* (non-Javadoc)
	 * @see org.bbop.client.Widgets.RefGWidgetInterface#configureItems()
	 */
	public abstract void configureItems();




	/* (non-Javadoc)
	 * @see org.bbop.client.Widgets.RefGWidgetInterface#createViewer()
	 */
	public void createViewer() {
		// TODO Auto-generated method stub
		setComponents();
		if (isSubComponents()) { 
			setSubComponents(); 
			configureSubComponents();
		}
		if (isItems()) {
			setItems();
			configureItems();
		}
		
		configureComponents();
		makeWidget();
		
		
		
	}




	/* (non-Javadoc)
	 * @see org.bbop.client.Widgets.RefGWidgetInterface#setComponents()
	 */
	public abstract void setComponents();




	/* (non-Javadoc)
	 * @see org.bbop.client.Widgets.RefGWidgetInterface#setItems()
	 */
	public abstract void setItems();




	/* (non-Javadoc)
	 * @see org.bbop.client.Widgets.RefGWidgetInterface#configureSubComponents()
	 */
	public abstract void configureSubComponents();




	/* (non-Javadoc)
	 * @see org.bbop.client.Widgets.RefGWidgetInterface#setSubComponents()
	 */
	public abstract void setSubComponents();




	/* (non-Javadoc)
	 * @see org.bbop.client.Widgets.RefGWidgetInterface#makeWidget()
	 */
	public abstract void makeWidget();
	
	
	public boolean isSubComponents() {
		return false;
	}
	
	public boolean isItems() {
		return false;
	}
	
	
	
	

}
