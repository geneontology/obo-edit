package org.oboedit.graph;

import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.swing.Box;
import javax.swing.ButtonGroup;
import javax.swing.ButtonModel;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;

import org.oboedit.gui.Preferences;
import org.oboedit.gui.components.GraphEditor;
import org.oboedit.gui.components.LinkDatabaseCanvas;

import org.apache.log4j.*;

public class ToolbarBehavior implements ViewBehavior {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ToolbarBehavior.class);

	protected LinkDatabaseCanvas canvas;

	protected JToolBar toolbar = new JToolBar();

	protected ToolbarButtonBehavior activeBehavior;

	protected List<ToolbarButtonBehavior> buttonBehaviors = new LinkedList<ToolbarButtonBehavior>();

	protected Map<ToolbarButtonBehavior, JToggleButton> behaviorMap = new HashMap<ToolbarButtonBehavior, JToggleButton>();

	protected ButtonGroup buttonGroup = new ButtonGroup();

	protected JPanel buttonPanel = new JPanel();

	public void install(LinkDatabaseCanvas canvas) {
		if (canvas instanceof GraphEditor) {
			this.canvas = canvas;
			toolbar.setCursor(Cursor.getDefaultCursor());
			((GraphEditor) canvas).getComponent().add(toolbar, "North");
			installDefaultBehaviors();
			if (buttonBehaviors.size() > 0)
				activateButtonBehavior(buttonBehaviors.get(0));
		}
	}

	protected void activateButtonBehavior(final ToolbarButtonBehavior behavior) {
		if (activeBehavior != null)
			activeBehavior.deactivate(canvas);
		activeBehavior = behavior;
		toolbar.removeAll();
		toolbar.add(buttonPanel);
		JComponent configComponent = behavior.getConfigurationPanel();
		if (configComponent != null) {
			toolbar.addSeparator();
			toolbar.add(configComponent);
		}
		toolbar.add(Box.createHorizontalGlue());
		toolbar.revalidate();
		toolbar.repaint();
		behavior.activate(canvas);
	}

	protected void installDefaultBehaviors() {
		AddNodeButtonBehavior addNodeBehavior = new AddNodeButtonBehavior();
		TextEditButtonBehavior textEditBehavior = new TextEditButtonBehavior();
		addNodeBehavior.setTextEditButtonBehavior(textEditBehavior);
		addButtonBehavior(new PickToolButtonBehavior());
		addButtonBehavior(new LinkingButtonBehavior());
		addButtonBehavior(addNodeBehavior);
		addButtonBehavior(new EraseButtonBehavior());
		addButtonBehavior(textEditBehavior);
		addButtonBehavior(new CloneButtonBehavior());
		ToolbarButtonBehavior defaultBehavior = buttonBehaviors.get(0);
		JToggleButton defaultButton = behaviorMap.get(defaultBehavior);
		if (defaultButton != null)
			defaultButton.setSelected(true);
	}

	public void uninstall(LinkDatabaseCanvas canvas) {
		if (canvas instanceof GraphEditor) {
			((GraphEditor) canvas).getComponent().remove(toolbar);
		}
		this.canvas = null;
	}

	public void addButtonBehavior(final ToolbarButtonBehavior b) {
		buttonBehaviors.add(b);
		JToggleButton button = new JToggleButton();
		if (b.getTooltip() != null)
			button.setToolTipText(b.getTooltip());
		button.setPreferredSize(new Dimension(20, 20));
		buttonGroup.add(button);
		button.setSelected(button.equals(b));
		if (b.getButtonLabel() != null)
			button.setText(b.getButtonLabel());
		if (b.getButtonIcon() != null)
			button.setIcon(b.getButtonIcon());
		button.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				activateButtonBehavior(b);
			}
		});
		behaviorMap.put(b, button);
		buttonPanel.add(button);
	}

	public void removeButtonBehavior(ToolbarButtonBehavior behavior) {
		buttonBehaviors.remove(behavior);
		buttonPanel.remove(behaviorMap.get(behavior));
	}
}
