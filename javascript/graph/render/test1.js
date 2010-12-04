//// (http://amigo.berkeleybop.org/amigo/panther/PTHR10004.tree).
//// AN0(AN1(AN2(XP_800359:0.687,XP_790652:0.774,XP_800360:0.695):0.473,AN6(Q7RKB3:1.366,Q7RBF2:1.208):0.223):1.0,Q747I8:1.0);

// Not all needed, but good habit?
bbop.core.require('bbop', 'core');
bbop.core.require('bbop', 'model');
bbop.core.require('bbop', 'model', 'tree');
bbop.core.require('bbop', 'render', 'phylo');

// Example graph hand loaded through tree.
var t = new bbop.model.tree.graph();

var an0 = new bbop.model.tree.node('AN0', 'AN0');
var an1 = new bbop.model.tree.node('AN1', 'AN1');
var an2 = new bbop.model.tree.node('AN2', 'AN2');
var an6 = new bbop.model.tree.node('AN6', 'AN6');
var xp9 = new bbop.model.tree.node('XP_800359', 'XP_800359');
var xp2 = new bbop.model.tree.node('XP_790652', 'XP_790652');
var xp0 = new bbop.model.tree.node('XP_800360', 'XP_800360');
var q7rk = new bbop.model.tree.node('Q7RKB3', 'Q7RKB3');
var q7rb = new bbop.model.tree.node('Q7RBF2', 'Q7RBF2');
var q747 = new bbop.model.tree.node('Q747I8', 'Q747I8');
t.add_node(an0);
t.add_node(an1);
t.add_node(an2);
t.add_node(an6);
t.add_node(xp9);
t.add_node(xp2);
t.add_node(xp0);
t.add_node(q7rk);
t.add_node(q7rb);
t.add_node(q747);

t.add_edge(new bbop.model.tree.edge(an0, an1, 1.0));
t.add_edge(new bbop.model.tree.edge(an0, q747, 1.0));
t.add_edge(new bbop.model.tree.edge(an1, an2, 0.473));
t.add_edge(new bbop.model.tree.edge(an1, an6, 0.223));
t.add_edge(new bbop.model.tree.edge(an2, xp9, 0.687));
t.add_edge(new bbop.model.tree.edge(an2, xp2, 0.774));
t.add_edge(new bbop.model.tree.edge(an2, xp0, 0.695));
t.add_edge(new bbop.model.tree.edge(an6, q7rk, 1.366));
t.add_edge(new bbop.model.tree.edge(an6, q7rb, 1.208));
var layout = t.layout();

// Run the layout after everything is loaded.
window.onload = function(){
    bbop.render.phylo.use_animation = true;
    bbop.render.phylo.display('test1', layout);
};
