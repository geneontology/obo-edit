//// (http://amigo.berkeleybop.org/amigo/panther/PTHR10004.tree).
//// AN0(AN1(AN2(XP_800359:0.687,XP_790652:0.774,XP_800360:0.695):0.473,AN6(Q7RKB3:1.366,Q7RBF2:1.208):0.223):1.0,Q747I8:1.0);

// Not all needed, but good habit?
bbop.core.require('bbop', 'core');
bbop.core.require('bbop', 'model');
bbop.core.require('bbop', 'model', 'tree');
bbop.core.require('bbop', 'render', 'phylo');

// Run the layout after everything is loaded.
window.onload = function(){

    // Set settable rendering properties.
    bbop.render.phylo.use_animation = true;

    // Example graph hand loaded through tree.
    var r = new bbop.render.phylo.renderer('test1');
    r.add_node('AN0');
    r.add_node('AN1');
    r.add_node('AN2');
    r.add_node('AN6');
    r.add_node('XP_800359');
    r.add_node('XP_790652');
    r.add_node('XP_800360');
    r.add_node('Q7RKB3');
    r.add_node('Q7RBF2');
    r.add_node('Q747I8');
    r.add_edge('AN0', 'AN1', 1.0);
    r.add_edge('AN0', 'Q747I8', 1.0);
    r.add_edge('AN1', 'AN2', 0.473);
    r.add_edge('AN1', 'AN6', 0.223);
    r.add_edge('AN2', 'XP_800359', 0.687);
    r.add_edge('AN2', 'XP_790652', 0.774);
    r.add_edge('AN2', 'XP_800360', 0.695);
    r.add_edge('AN6', 'Q7RKB3', 1.366);
    r.add_edge('AN6', 'Q7RBF2', 1.208);

    r.display();
};
