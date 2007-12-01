package org.bbop.framework;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.util.LinkedList;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.Icon;
import javax.swing.JOptionPane;


public abstract class AbstractComponentFactory<T extends GUIComponent> implements
		GUIComponentFactory<T> {
	protected class MessageAction extends AbstractAction {

		private Component parentComponent;

		private String message;

		private String name;

		public MessageAction(Component parentComponent, String name, Icon icon,
				String message) {
			super(name, icon);
			this.message = message;
			this.name = name;
			this.parentComponent = parentComponent;
		}

		public void actionPerformed(ActionEvent actionEvent) {
			JOptionPane.showMessageDialog(parentComponent, message, name,
					JOptionPane.INFORMATION_MESSAGE);
		}

	}

	public T createComponent(String id) {		
		T out = doCreateComponent(id);
		out.setTitle(getName());
		return out;
	}

	public abstract T doCreateComponent(String id);
	
	public boolean getPreferSeparateWindow() {
		return false;
	}

	public boolean isSingleton() {
		return false;
	}

	public boolean showInMenus() {
		return true;
	}
	
	public boolean isRestoreOnStartup() {
		return true;
	}
}
