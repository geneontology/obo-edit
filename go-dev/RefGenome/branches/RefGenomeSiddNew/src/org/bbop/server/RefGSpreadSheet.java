package org.bbop.server;


import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.gdata.client.spreadsheet.FeedURLFactory;
import com.google.gdata.client.spreadsheet.SpreadsheetService;
import com.google.gdata.data.BaseEntry;
import com.google.gdata.data.spreadsheet.ListEntry;
import com.google.gdata.data.spreadsheet.ListFeed;
import com.google.gdata.data.spreadsheet.SpreadsheetEntry;
import com.google.gdata.data.spreadsheet.SpreadsheetFeed;
import com.google.gdata.data.spreadsheet.WorksheetEntry;
import com.google.gdata.util.AuthenticationException;
import com.google.gdata.util.ServiceException;

public class RefGSpreadSheet {

	/** Our view of Google Spreadsheets as an authenticated Google user. */
	private SpreadsheetService service;

	/** The URL of the list feed for the selected spreadsheet. */
	private URL listFeedUrl;

	/** A factory that generates the appropriate feed URLs. */
	private FeedURLFactory factory;
	
	

	/** Regex for selecting spreadsheet */
	Pattern refgSheetRegex = Pattern.compile("Reference\\s+Genome\\s+Targets",
			Pattern.CASE_INSENSITIVE);

	/** Regex for selecting worksheet */
	Pattern refgWsheetRegex = Pattern.compile("Disease\\s+gene\\s+annotations",
			Pattern.CASE_INSENSITIVE);

	/** Column names to capture */
	final String[] colNames = { "hsgenesymbol", "omimdiseasename",
			"entrezgeneid", "refseqid", "uniprotkbac.", "dateaddedtorefgenomelist", "completiontarget", "curatorfororthologset" };

	final HashMap<String, String> colMaps = new HashMap<String, String>();

	List<HashMap<String, String>> result;

	public RefGSpreadSheet() {
		service = new SpreadsheetService("RefGenome demo");
		factory = FeedURLFactory.getDefault();
		result = new ArrayList<HashMap<String, String>>();

		for (String name : colNames) {
			colMaps.put(name, "none");
		}
	}

	public void login(String username, String password)
			throws AuthenticationException {
		// Authenticate
		service.setUserCredentials(username, password);

	}

	public void loadSheet() throws ServiceException {

		SpreadsheetFeed feed = service.getFeed(
				factory.getSpreadsheetsFeedUrl(), SpreadsheetFeed.class);

		//Now get the spreadsheet of interest
		List spreadsheets = feed.getEntries();
		int spreadsheetIndex = getIndex(spreadsheets, refgSheetRegex);
		SpreadsheetEntry spreadsheet = (SpreadsheetEntry) spreadsheets
				.get(spreadsheetIndex);

		//Now get the worksheet of interest
		List worksheets = spreadsheet.getWorksheets();
		int worksheetIndex = getIndex(worksheets, refgWsheetRegex);

		WorksheetEntry worksheet = (WorksheetEntry) worksheets
				.get(worksheetIndex);
		listFeedUrl = worksheet.getListFeedUrl();
	}

	public int getIndex(List entries, Pattern pattern) {
		int index = 0;
		for (int i = 0; i < entries.size(); i++) {
			BaseEntry entry = (BaseEntry) entries.get(i);
			String name = entry.getTitle().getPlainText();
			Matcher m = pattern.matcher(name);
			if (m.find()) {
				return i;
			}
		}
		return index;
	}

	public void fetchData() {
		try {
			ListFeed feed = (ListFeed) service.getFeed(listFeedUrl,
					ListFeed.class);
			for (ListEntry entry : feed.getEntries()) {
				storeData(entry);
			}

		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ServiceException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	public void storeData(ListEntry entry) {
		HashMap<String, String> resultMap = new HashMap<String, String>();

		for (String tagName : entry.getCustomElements().getTags()) {
			
			if (colMaps.containsKey(tagName)) {
				String value = entry.getCustomElements().getValue(tagName);
				if (value == null || value.length() == 0) {
					value = "";
				}
				else {
					value = value.trim();
				}
				resultMap.put(tagName, value);
				result.add(resultMap);
			}
		}
		
	}

	public void printData() {
		for (HashMap<String, String> dataMap : result) {
			for (String name : colNames) {
					System.out.print(name + ":" + dataMap.get(name) + "\t");
			}
			System.out.println();

		}
	}

	/**
	 * @param fetches
	 *            data from Reference genome target spreadsheet
	 */
	

}
