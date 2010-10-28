This is indended to be a fresh start for JS libraries initially
intended to act as a client-side GraphViz (Sugiyama layout)
replacement and general graph model. The last active amigo2
development was likely in amigo/lib/javascript/beta, but I'm not
completely sure. There was a small refresh started in
amigo/javascript/org/bbop, but nothing with much traction and mostly
involving adding a test layer and session modules.

Considering some of the repetition that comes up, I'm tempted to
shadow some development in Parenscript to see if that might make it go
faster.

This restart will base off of the amigo/bbop code, and bring in last
good sugiyama and rendermonkey as needed. We should also try and get
some of the tree branch render code that briefly appeared in the
design of the new term details page (AmiGO 1.8)--it might be a jumping
off point for the phylotree. Also, there are some nice bits in
amigo/bbop's amigo and ajax.

General goals are:

* Retool to operate with Rhino in addition to Spidermonkey [DONE]

As support for Spidermonkey has always been marginal in packages and
the possible usefulness Rhino increasing, the libraries should be
retooled to not collide with its namespeces. Development and debugging
will mostly occur with Rhino, occasional spot-checking in
Spidermonkey. This will likely mean a general flattening of the
library namespace.

* Light reimplementation of the graph/model based on the last amigo code [DONE]

We'll start there, but strip out a lot of the unecessary OBD frippery.

* General applicability

Should be usable for phylo trees as well as the gene ontology. Should
also try to be fast/robust enough to handle the whole of GO.

* Abstract rendering

Graph code should be completely separate from layout (sugiyama) and
render (rendermonkey) code. However, the graph model should contain
implicit relations code, and either embedded or parallel meta
information (to allow things like arc length in phylo trees).

Abstract objects will take the plain graph (and possibly additional
information) and turn them into an intermediate form for the final
rendering target. For example: GO hierarchical, GO planar,
phylo. These forms would then be fed into renders; example:
hierarchical -> text, phylo -> HTML.
