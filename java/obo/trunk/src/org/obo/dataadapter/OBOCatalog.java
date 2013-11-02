package org.obo.dataadapter;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * Simple catalog to map import declaration strings to a local address.
 */
public class OBOCatalog {
	
	private static final String CATALOG_FILE_NAME = "oboedit.catalog";

	private Map<String, String> mappings;
	
	public OBOCatalog(File folder) {
		mappings = Collections.emptyMap();
		if (folder.isDirectory()) {
			File catalogFile = new File(folder, CATALOG_FILE_NAME);
			if (catalogFile.isFile() && catalogFile.canRead()) {
				mappings = new HashMap<String, String>();
				BufferedReader reader = null;
				try {
					reader = new BufferedReader(new FileReader(catalogFile));
					String line;
					while ((line = reader.readLine()) != null) {
						line = line.trim();
						int totalLength = line.length();
						if (totalLength >= 3 && line.charAt(0) != '#') {
							String[] strings = split(line);
							if (strings != null && strings.length == 2) {
								mappings.put(strings[0], strings[1]);
							}
						}
					}
				} catch (IOException e) {
					// ignore silently
				}
				finally {
					if (reader != null) {
						try {
							reader.close();
						} catch (IOException e) {
							// close quietly
						}
					}
				}
			}
		}
	}
	
	private static String[] split(String s) {
		// split by first whitespace or tab
		int totalLength = s.length(); 
		int splitPos = -1;
		for (int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			if (Character.isWhitespace(c)) {
				splitPos = i;
				break;
			}
		}
		if (splitPos > 0 && splitPos < (totalLength - 1)) {
			String[] strings = new String[2];
			String local = s.substring(splitPos).trim();
			if (local.length() > 0) {
				strings[0] = s.substring(0, splitPos);
				strings[1] = local;
				return strings;
			}
		}
		return null;
	}
	
	public String map(String input) {
		String mapped = mappings.get(input);
		if (mapped != null) {
			return mapped;
		}
		return input;
	}
}
