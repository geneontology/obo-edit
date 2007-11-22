package org.oboedit.example;

import java.awt.Color;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import javax.swing.JComponent;
import org.obo.datamodel.IdentifiedObject;
import org.oboedit.graph.AbstractFetchTask;
import org.oboedit.gui.FilteredRenderable;
import org.oboedit.gui.filter.BackgroundColorSpecField;
import org.oboedit.gui.filter.GeneralRendererSpec;
import org.oboedit.gui.filter.HTMLSpecField;
import org.oboedit.gui.filter.HeatmapColor;

public class SimpleLineNumberFetchBehaviorTask extends AbstractFetchTask<Integer> {

	public SimpleLineNumberFetchBehaviorTask() {
		super(Integer.class);
	}

	@Override
	protected String getBehaviorID() {
		return "line_number_fetch";
	}

	protected String fileLoc = "/Users/jrichter/ontology/gene_ontology_edit.obo";

	@Override
	protected Integer getValue(IdentifiedObject io) {
		int lineNum = 0;
		boolean found = false;
		try {
			BufferedReader reader = new BufferedReader(new FileReader(fileLoc));
			String line;
			while ((line = reader.readLine()) != null) {
				lineNum++;
				if (line.startsWith("id: " + io.getID())) {
					found = true;
					break;
				}
			}
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		if (!found)
			lineNum = -1;
		return lineNum;
	}

	@Override
	protected GeneralRendererSpec getFetchedRenderer(FilteredRenderable canvas,
			String valueVar) {
		return new GeneralRendererSpec(HTMLSpecField.FIELD,
				"$term$<hr><center><font color=white>defined on line $"
						+ valueVar + "$</font></center>",
				BackgroundColorSpecField.FIELD, new HeatmapColor(Color.yellow,
						Color.red, valueVar));
	}

	@Override
	protected GeneralRendererSpec getPendingRenderer(FilteredRenderable canvas,
			String valueVar) {
		return new GeneralRendererSpec(HTMLSpecField.FIELD,
				"$term$<hr><center><font color=white><i>Loading...</i></font></center>");
	}

	@Override
	protected JComponent getConfigurationPanel() {
		return null;
	}

	@Override
	protected String getName() {
		return "Line Number Renderer Plugin";
	}

}
