package org.oboedit.gui.widget;

import javax.swing.*;
import javax.swing.border.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.util.*;

import org.obo.filters.*;

public class FilterRenderEditor extends JPanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	// protected CompoundEditable filterBuilder = new FilterBuilder();
	protected CompoundEditable filterBuilder; // = new KeywordFilterBuilder();
	protected SpecEditor specEditor = new ObjectSpecEditor();

	protected boolean specEditorVisible;
	protected boolean linkMode = false;
	protected TitledBorder titledBorder = new TitledBorder("Term filter");
	protected TitledBorder specBorder = new TitledBorder("Render options");
	protected Color buttonColor;
	protected Collection actionListeners = new LinkedList();
	protected boolean showCompoundFilter = false;

	public void addActionListener(ActionListener listener) {
		filterBuilder.addActionListener(listener);
		actionListeners.add(listener);
	}

	public void removeActionListener(ActionListener listener) {
		filterBuilder.removeActionListener(listener);
		actionListeners.remove(listener);
	}

	@Override
	public void setEnabled(boolean enabled) {
		((JComponent) specEditor).setEnabled(enabled);
		((JComponent) filterBuilder).setEnabled(enabled);
		super.setEnabled(enabled);
	}

	public void setSpecEditorVisible(boolean specEditorVisible) {
		if (specEditorVisible != this.specEditorVisible) {
			if (this.specEditorVisible) {
				remove((JComponent) specEditor);
			} else {
				add((JComponent) specEditor, "East");
			}
			validate();
			repaint();
		}
		this.specEditorVisible = specEditorVisible;
	}

	public void setShowCompoundFilter(boolean showCompoundFilter) {
		this.showCompoundFilter = showCompoundFilter;
		filterBuilder.setShowCompoundFilter(showCompoundFilter);
	}

	public FilterRenderEditor() {
		this(false);
	}

	public void setButtonColor(Color buttonColor) {
		this.buttonColor = buttonColor;
		filterBuilder.setButtonColor(buttonColor);
	}

	public void acceptEdits() {
		filterBuilder.acceptEdits();
	}

	@Override
	public void setFont(Font font) {
		super.setFont(font);
		if (specEditor != null)
			((JComponent) specEditor).setFont(font);
		if (filterBuilder != null)
			((JComponent) filterBuilder).setFont(font);
		if (titledBorder != null)
			titledBorder.setTitleFont(font);
		if (specBorder != null)
			specBorder.setTitleFont(font);
	}

	public FilterRenderEditor(boolean linkMode) {
		setLayout(new BorderLayout());
		setOpaque(false);

		this.linkMode = linkMode;

		if (linkMode) {
			specEditor = new LinkSpecEditor();
			titledBorder.setTitle("Link filter");
			setFilterBuilder(new FilterBuilder());
		} else {
			specEditor = new ObjectSpecEditor();
			titledBorder.setTitle("Term filter");
			setFilterBuilder(new KeywordFilterBuilder());
		}
		((JComponent) specEditor).setBorder(specBorder);
	}

	protected void setFilterBuilder(CompoundEditable filterBuilder) {
		if (linkMode) {
			filterBuilder.setFilterFactory(new LinkFilterFactory());
		} else {
			filterBuilder.setFilterFactory(new ObjectFilterFactory());
		}
		filterBuilder.setShowCompoundFilter(showCompoundFilter);

		if (this.filterBuilder != null) {
			this.filterBuilder.acceptEdits();
			filterBuilder.setFilter(this.filterBuilder.getFilter());
			((JComponent) this.filterBuilder).setBorder(null);
			remove((JComponent) this.filterBuilder);
			Iterator it = actionListeners.iterator();
			while (it.hasNext()) {
				ActionListener l = (ActionListener) it.next();
				this.filterBuilder.removeActionListener(l);
			}
		}

		((JComponent) filterBuilder).setBorder(titledBorder);
		Iterator it = actionListeners.iterator();
		while (it.hasNext()) {
			filterBuilder.addActionListener((ActionListener) it.next());
		}
		if (buttonColor != null)
			filterBuilder.setButtonColor(buttonColor);
		add((Component) filterBuilder, "Center");
		this.filterBuilder = filterBuilder;
	}

	public RenderSpec getSpec() {
		return specEditor.getSpec();
	}

	public Filter getFilter() {
		acceptEdits();
		return filterBuilder.getFilter();
	}

	public void setSpec(RenderSpec spec) {
		specEditor.setSpec(spec);
	}

	public void setFilter(Filter filter) {
		filterBuilder.setFilter(filter);
	}
}
