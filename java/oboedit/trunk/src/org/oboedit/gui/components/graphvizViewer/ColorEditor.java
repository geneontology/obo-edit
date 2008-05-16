package org.oboedit.gui.components.graphvizViewer;

import java.awt.Component;

import javax.swing.BoxLayout;
import javax.swing.JColorChooser;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import org.bbop.swing.GenericEditorComponent;
import org.bbop.swing.ListEditor;

import org.apache.log4j.*;

public class ColorEditor extends JPanel implements GenericEditorComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ColorEditor.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	JColorChooser textColorChooser = new JColorChooser();
	JColorChooser lineColorChooser = new JColorChooser();

	JTabbedPane tabbedPane = new JTabbedPane();

	protected ListEditor editor;

	protected ChangeListener cl = new ChangeListener() {
		public void stateChanged(ChangeEvent e) {
			editor.commit();
		}
	};

	public ColorEditor() {
		lineColorChooser.setFont(getFont());
		textColorChooser.setFont(getFont());

		setLayout(new BoxLayout(ColorEditor.this, BoxLayout.Y_AXIS));

		attachListeners();
	}

	protected void attachListeners() {
		textColorChooser.getSelectionModel().addChangeListener(cl);
		lineColorChooser.getSelectionModel().addChangeListener(cl);
	}

	public Object createNewValue() {
		return null;
	}

	public void load(Object o) {
		removeAll();
		removeListeners();
		if (o instanceof TypeColorPair) {
			tabbedPane.removeAll();
			tabbedPane.addTab("Type label color", textColorChooser);
			tabbedPane.addTab("Type line color", lineColorChooser);
			TypeColorPair cp = (TypeColorPair) o;
			textColorChooser.setColor(cp.getPair().label);
			lineColorChooser.setColor(cp.getPair().edge);
			add(tabbedPane);
		} else if (o instanceof NamedColor) {
			tabbedPane.removeAll();
			NamedColor nc = (NamedColor) o;
			textColorChooser.setColor(nc.getColor());
			add(textColorChooser);
		}
		attachListeners();
	}

	protected void removeListeners() {
		textColorChooser.getSelectionModel().removeChangeListener(cl);
		lineColorChooser.getSelectionModel().removeChangeListener(cl);
	}

	public void setMasterComponent(Component c) {
		if (c instanceof ListEditor)
			editor = (ListEditor) c;
	}



	public void store(Object o) {
		if (o instanceof TypeColorPair) {
			TypeColorPair cp = (TypeColorPair) o;
			cp.getPair().label = textColorChooser.getColor();
			cp.getPair().edge = lineColorChooser.getColor();
		} else if (o instanceof NamedColor) {
			NamedColor nc = (NamedColor) o;
			nc.color = textColorChooser.getColor();
		}
	}
}
