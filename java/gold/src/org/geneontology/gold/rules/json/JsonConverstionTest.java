package org.geneontology.gold.rules.json;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

import junit.framework.TestCase;

public class JsonConverstionTest extends TestCase {

	
	public void testConversion(){
		AnnotationVoilationForJson av = new AnnotationVoilationForJson("v1", "a1", "r1", 1, "f1");
		
		List<AnnotationVoilationForJson> list = new ArrayList<AnnotationVoilationForJson>();
		list.add(av);

		av = new AnnotationVoilationForJson("v2", "a2", "r1", 2, "f1");
		list.add(av);
		
		av = new AnnotationVoilationForJson("v3", "a3", "r1", 2, "f1");
		list.add(av);
		
		Type type = new TypeToken<List<AnnotationVoilationForJson>>(){}.getType();
		
		Gson gson = new Gson();
		
		String gsonText= gson.toJson(list, type);
		
		System.out.println(gsonText);
		
		ErrorForJson error = new ErrorForJson("An error occured");
		
		gsonText = gson.toJson(error);
		
		System.out.println(gsonText);
		
		
	}
	
}
