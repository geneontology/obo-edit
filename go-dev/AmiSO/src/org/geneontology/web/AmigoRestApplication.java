package org.geneontology.web;

import java.net.URL;
import java.sql.SQLException;

import org.obd.query.Shard;
import org.obd.query.impl.ChadoSQLShard;
import org.obd.query.impl.MultiShard;
import org.obd.query.impl.MutableOBOSessionShard;
import org.obd.query.impl.OBDSQLShard;
import org.obd.ws.*;
import org.obd.ws.OBDRestApplication.Config;
import org.restlet.Application;
import org.restlet.Component;
import org.restlet.Context;
import org.restlet.Directory;
import org.restlet.Restlet;
import org.restlet.Router;
import org.restlet.data.LocalReference;
import org.restlet.data.Protocol;
import org.restlet.data.Reference;

import org.geneontology.web.AmigoHomeResource;
import org.geneontology.web.NodeDetailResource;

public class AmigoRestApplication extends OBDRestApplication {

	public static void main(String[] args) throws Exception {
		startServer(args);
	}

	public static void startServer(String[] args) throws Exception {
		// Create a component
		Component component = new Component();
		component.getClients().add(Protocol.FILE);
		component.getClients().add(Protocol.JAR);
		component.getClients().add(Protocol.CLAP);

		OBDRestApplication application = new AmigoRestApplication(
				component.getContext());

		Config config = parseMainArguments(args);
		component.getServers().add(Protocol.HTTP, config.getPort());

		application.setShard(config.getShard());

		// Attach the application to the component and start it
		component.getDefaultHost().attach("", application);

		component.start();
	}

	public AmigoRestApplication() {
		super();
	}

	public AmigoRestApplication(Context context) {
		super(context);
	}

	@Override
	public Restlet createRoot() {
		Router router = new Router(getContext());

		router.attach("/{format}/entity/{id}", NodeDetailResource.class);


		// Add a route for the top-level resource

		router.attach("/", AmigoHomeResource.class).
		getTemplate().setMatchingMode(org.restlet.util.Template.MODE_EQUALS);
		router.attach("", AmigoHomeResource.class);
		router.attach("/help", HomeResource.class);
		router.attach("/help/", HomeResource.class);
		router.attach("/help/{id}", HomeResource.class);

		router.attach("/{format}/help/", HomeResource.class);

		router.attach("/{format}/metadata/", ShardMetadataResource.class);
		router.attach("/pages/{page}.html", PageResource.class);
		router.attach("/usecases/{usecase}.html", PageResource.class);

		// TODO: use PageResource
		String base = "/org/geneontology/web/pages";
		
		Directory cssDirectory = 
			new Directory(getContext(), 
					LocalReference.createClapReference(LocalReference.CLAP_CLASS, 
							base+"/css/"));
		router.attach("/css/", cssDirectory);

		Directory imagesDirectory = new Directory(getContext(), 
				LocalReference.createClapReference(LocalReference.CLAP_CLASS, 
						base+"/images/"));
		router.attach("/images/", imagesDirectory);

		Directory jsDirectory = new Directory(getContext(), 
				LocalReference.createClapReference(LocalReference.CLAP_CLASS, 
						base+"/js/"));
		router.attach("/js/", jsDirectory);

		// Add a route for node resources
		router.attach("/{format}/nodes/{id}", NodeResource.class);

		// Add routes for resources accessed via a node
		// should it be possible to append all statements URLs with /graph?
		router.attach("/{format}/nodes/{id}/statements", StatementsResource.class);
		router.attach("/{format}/nodes/{id}/statements/{aspect}", StatementsResource.class);
		// is this a politically correct way of doing a filter in REST?
		router.attach("/{format}/nodes/{id}/statements/{aspect}/{relation}", StatementsResource.class);
		// class-expression composite description
		// (here we use the term "description" in the description logic sense)
		router.attach("/{format}/nodes/{id}/description", DescriptionResource.class);

		router.attach("/{format}/nodes/{id}/blast", ScoredNodesResource.class);

		// Add a route for graph-by-node resources
		router.attach("/{format}/nodes/{id}/graph", GraphResource.class);
		router.attach("/{format}/nodes/{id}/graph/{aspect}", GraphResource.class);

		// Add a route for source node resources
		router.attach("/{format}/sources", SourcesResource.class);
		router.attach("/{format}/sources/{id}", SourceResource.class);
		router.attach("/{format}/sources/{id}/nodes", SourceResource.class);
		router.attach("/{format}/sources/{id}/statements", SourceResource.class);
		router.attach("/{format}/sources/{id}/graph", SourceResource.class);
		router.attach("/{format}/sources/{id}/annotations", SourceResource.class);

		// Add a route for node resources
		router.attach("/{format}/search/{operator}/{term}", NodesBySearchResource.class);
		router.attach("/{format}/search/{operator}/{term}/statements", StatementsBySearchResource.class);
		router.attach("/{format}/search/{operator}/{term}/statements/{aspect}", StatementsBySearchResource.class);

		router.attach("/{format}/hset/{id}", NestedAnnotationResource.class);



		return router;
	}



}

