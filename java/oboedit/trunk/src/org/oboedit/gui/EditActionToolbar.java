package org.oboedit.gui;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Font;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.util.LinkedList;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JToolBar;
import javax.swing.KeyStroke;
import javax.swing.ListCellRenderer;
import javax.swing.UIManager;

import org.bbop.util.ObjectUtil;
import org.oboedit.controller.EditActionManager;
import org.oboedit.gui.components.OntologyEditorConfiguration;

public class EditActionToolbar extends JToolBar {

	protected JComponent masterPanel;
	protected int showToolbar = OntologyEditorConfiguration.SHOW_TOOLBAR_ON_HOTKEY;
	protected JComboBox gestureBox = new JComboBox();
	protected List<ActionListener> listeners = new LinkedList<ActionListener>();
	protected InputHandlerI currentHandler;
	protected AbstractSelectableHandlerBridge inputListener;
	protected String toolbarPosition;
	protected JLabel label = new JLabel("Current drag and drop behavior");

	protected static class BoxRenderer extends JPanel implements
			ListCellRenderer {
		protected JLabel nameLabel = new JLabel();
		protected JLabel shortcutLabel = new JLabel();

		public BoxRenderer() {
			setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
			add(nameLabel);
			add(Box.createHorizontalGlue());
			add(shortcutLabel);
			shortcutLabel
					.setFont(shortcutLabel.getFont().deriveFont(Font.BOLD));
		}

		public Component getListCellRendererComponent(JList list, Object value,
				int index, boolean isSelected, boolean cellHasFocus) {
			if (isSelected)
				setBackground(Preferences.defaultSelectionColor());
			else
				setBackground(null);
			InputHandlerI handler = (InputHandlerI) value;
			nameLabel.setText(handler.getName());
			KeyStroke accelerator = handler.getShortcut();
			String acceleratorDelimiter = UIManager
					.getString("MenuItem.acceleratorDelimiter");
			if (acceleratorDelimiter == null
					|| acceleratorDelimiter.length() == 0) {
				acceleratorDelimiter = "+";
			}
			String acceleratorText = "";
			if (accelerator != null) {
				int modifiers = accelerator.getModifiers();
				if (modifiers > 0) {
					acceleratorText = getKeyModifiersText(modifiers);
					// acceleratorText += "-";
					acceleratorText += acceleratorDelimiter;
				}

				int keyCode = accelerator.getKeyCode();
				if (keyCode != 0) {
					acceleratorText += KeyEvent.getKeyText(keyCode);
				} else {
					acceleratorText += accelerator.getKeyChar();
				}
			}
			shortcutLabel.setText(acceleratorText);
			validate();
			return this;
		}

	}

	public static String getKeyModifiersText(int modifiers) {
		StringBuffer buf = new StringBuffer();
		if ((modifiers & InputEvent.META_MASK) != 0) {
			buf.append("" + ((char) Integer.parseInt("2318", 16)));
			buf.append("+");
		}
		if ((modifiers & InputEvent.CTRL_MASK) != 0) {
			buf.append(Toolkit.getProperty("AWT.control", "Ctrl"));
			buf.append("+");
		}
		if ((modifiers & InputEvent.ALT_MASK) != 0) {
			buf.append(Toolkit.getProperty("AWT.alt", "Alt"));
			buf.append("+");
		}
		if ((modifiers & InputEvent.SHIFT_MASK) != 0) {
			buf.append(Toolkit.getProperty("AWT.shift", "Shift"));
			buf.append("+");
		}
		if ((modifiers & InputEvent.ALT_GRAPH_MASK) != 0) {
			buf.append(Toolkit.getProperty("AWT.altGraph", "Alt Graph"));
			buf.append("+");
		}
		if ((modifiers & InputEvent.BUTTON1_MASK) != 0) {
			buf.append(Toolkit.getProperty("AWT.button1", "Button1"));
			buf.append("+");
		}
		if (buf.length() > 0) {
			buf.setLength(buf.length() - 1); // remove trailing '+'
		}
		return buf.toString();
	}

	public EditActionToolbar(JComponent masterPanel,
			AbstractSelectableHandlerBridge inputListener, boolean canFloat) {
		this.masterPanel = masterPanel;
		this.inputListener = inputListener;
		add(label);
		add(Box.createHorizontalStrut(5));
		add(gestureBox);
		gestureBox.setRenderer(new BoxRenderer());
		gestureBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (getShowToolbar() == OntologyEditorConfiguration.SHOW_TOOLBAR_ON_HOTKEY
						&& ObjectUtil.equals(gestureBox.getSelectedItem(),
								EditActionManager.getManager()
										.getDefaultInputHandler()))
					showToolbar(false);
			}
		});
		setFloatable(canFloat);
		updateGestureList();
	}

	public void updateGestureList() {
		gestureBox.removeAllItems();
		for (final InputHandlerI handler : EditActionManager.getManager()
				.getInputHandlers()) {
			gestureBox.addItem(handler);
			String key = "activate_gesture_" + handler.getID();
			masterPanel.getInputMap(
					JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(
					handler.getShortcut(), key);
			masterPanel.getActionMap().put(key, new AbstractAction() {
				public void actionPerformed(ActionEvent e) {
					setCurrentHandler(handler);
					if (getShowToolbar() == OntologyEditorConfiguration.SHOW_TOOLBAR_ON_HOTKEY)
						showToolbar(true);
				}
			});
		}
		gestureBox.setSelectedItem(EditActionManager.getManager()
				.getDefaultInputHandler());
	}

	public InputHandlerI getCurrentHandler() {
		return (InputHandlerI) gestureBox.getSelectedItem();
	}

	public void setCurrentHandler(InputHandlerI currentHandler) {
		inputListener.setDefaultInputHandler(currentHandler);
		removeAllListeners();
		gestureBox.setSelectedItem(currentHandler);
		restoreAllListeners();
	}

	public void addActionListener(ActionListener actionListener) {
		listeners.add(actionListener);
		gestureBox.addActionListener(actionListener);
	}

	public void removeActionListener(ActionListener actionListener) {
		listeners.remove(actionListener);
		gestureBox.removeActionListener(actionListener);
	}

	protected void removeAllListeners() {
		for (ActionListener listener : listeners) {
			gestureBox.removeActionListener(listener);
		}
	}

	protected void restoreAllListeners() {
		for (ActionListener listener : listeners) {
			gestureBox.addActionListener(listener);
		}
	}

	public int getShowToolbar() {
		return showToolbar;
	}

	public void setShowToolbar(int showToolbar) {
		this.showToolbar = showToolbar;
		showToolbar(showToolbar == OntologyEditorConfiguration.SHOW_TOOLBAR_ALWAYS);
	}

	protected void showToolbar(boolean show) {
		String toolbarPosition = getToolbarPosition();
		setToolbarPosition(toolbarPosition, show);
	}

	protected void setToolbarPosition(String toolbarPosition,
			boolean showToolbar) {
		if (showToolbar) {
			if (toolbarPosition == null)
				toolbarPosition = BorderLayout.NORTH;
			masterPanel.remove(this);
			masterPanel.add(this, toolbarPosition);
		} else
			masterPanel.remove(this);
		masterPanel.validate();
		masterPanel.repaint();
	}

	protected void updateToolbarPosition() {
		BorderLayout layout = (BorderLayout) masterPanel.getLayout();
		String pos = (String) layout.getConstraints(this);
		if (pos != null)
			toolbarPosition = pos;
	}

	public String getToolbarPosition() {
		updateToolbarPosition();
		return toolbarPosition;
	}

	public void setToolbarPosition(String toolbarPosition) {
		this.toolbarPosition = toolbarPosition;
		setToolbarPosition(
				toolbarPosition,
				getShowToolbar() == OntologyEditorConfiguration.SHOW_TOOLBAR_ALWAYS);
	}
}
