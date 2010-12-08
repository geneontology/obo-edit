//// (http://amigo.berkeleybop.org/amigo/panther/PTHR10004.tree).
//// AN0(AN1(AN2(XP_800359:0.687,XP_790652:0.774,XP_800360:0.695):0.473,AN6(Q7RKB3:1.366,Q7RBF2:1.208):0.223):1.0,Q747I8:1.0);

// Not all needed, but good habit?
bbop.core.require('bbop', 'core');
bbop.core.require('bbop', 'model');
bbop.core.require('bbop', 'model', 'tree');
bbop.core.require('bbop', 'render', 'phylo');

// Run the layout after everything is loaded.
window.onload = function(){

    // Example graph hand loaded through tree.
    var r1 = new bbop.render.phylo.renderer('test1');
    r1.add_node('AN0');
    r1.add_node('AN1');
    r1.add_node('AN2');
    r1.add_node('AN6');
    r1.add_node('XP_800359');
    r1.add_node('XP_790652');
    r1.add_node('XP_800360');
    r1.add_node('Q7RKB3');
    r1.add_node('Q7RBF2');
    r1.add_node('Q747I8');
    r1.add_edge('AN0', 'AN1', 1.0);
    r1.add_edge('AN0', 'Q747I8', 1.0);
    r1.add_edge('AN1', 'AN2', 0.473);
    r1.add_edge('AN1', 'AN6', 0.223);
    r1.add_edge('AN2', 'XP_800359', 0.687);
    r1.add_edge('AN2', 'XP_790652', 0.774);
    r1.add_edge('AN2', 'XP_800360', 0.695);
    r1.add_edge('AN6', 'Q7RKB3', 1.366);
    r1.add_edge('AN6', 'Q7RBF2', 1.208);

    // Set settable rendering properties.
    r1.use_animation = true;

    // Display.
    r1.display();

    //
    var r2 = new bbop.render.phylo.renderer('test2');
    r2.add_node('A0');
    r2.add_node('D0');
    r2.add_node('D1');
    r2.add_node('D2');
    r2.add_node('D3');
    r2.add_node('D4');
    r2.add_edge('A0', 'D0', 1.0);
    r2.add_edge('A0', 'D1', 1.5);
    r2.add_edge('A0', 'D2', 2.0);
    r2.add_edge('A0', 'D3', 2.5);
    r2.add_edge('A0', 'D4', 3.0);
    r2.box_width = 100;
    r2.box_height = 20;    
    r2.display();

    //
    var r3 = new bbop.render.phylo.renderer('test3');
    r3.add_node('A0');
    r3.add_node('D0');
    r3.add_node('D1');
    r3.add_node('D2');
    r3.add_node('D3');
    r3.add_node('D4');
    r3.add_edge('A0', 'D0', 1.0);
    r3.add_edge('A0', 'D1', 1.5);
    r3.add_edge('A0', 'D2', 2.0);
    r3.add_edge('A0', 'D3', 2.5);
    r3.add_edge('A0', 'D4', 3.0);
    r3.box_width = 20;
    r3.box_height = 20;    
    r3.display();
};
