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
    var r0 = new bbop.render.phylo.renderer('test0', true);

    r0.add_node('AN0');

    r0.add_node('AN1');
    r0.add_node('EAL71324');
    r0.add_edge('AN0', 'AN1', 0.48);
    r0.add_edge('AN0', 'EAL71324', 0.48);

    r0.add_node('AN2');
    r0.add_node('AN36');
    r0.add_edge('AN1', 'AN2', 0.563);
    r0.add_edge('AN1', 'AN36', 0.439);

    r0.add_node('AN38');
    r0.add_node('Q9U999');
    r0.add_edge('AN36', 'AN38', 0.31);
    r0.add_edge('AN36', 'Q9U999', 0.379);

    r0.add_node('AGAP003682-PA');
    r0.add_node('AGAP003681-PA');
    r0.add_edge('AN38', 'AGAP003682-PA', 2.0);
    r0.add_edge('AN38', 'AGAP003681-PA', 0.755);

    r0.add_node('AN3');
    r0.add_node('AN32');
    r0.add_edge('AN2', 'AN3', 0.025);
    r0.add_edge('AN2', 'AN32', 0.0);

    r0.add_node('XP_795551');
    r0.add_node('XP_001193149');
    r0.add_node('XP_001200298');
    r0.add_edge('AN32', 'XP_795551', 1.68);
    r0.add_edge('AN32', 'XP_001193149', 0.411);
    r0.add_edge('AN32', 'XP_001200298', 0.411);

    // r0.add_node('AN4');
    // r0.add_node('AN5');
    // r0.add_node('AN6');
    // r0.add_node('AN7');
    // r0.add_node('AN8');
    // r0.add_node('AN9');
    // r0.add_node('AN10');
    // r0.add_node('AN11');
    // r0.add_node('AN12');
    // r0.add_node('P42858');
    // r0.add_node('ENSPTRP00000027313');
    // r0.add_node('ENSMMUP00000011008');
    // r0.add_node('AN16');
    // r0.add_node('P42859');
    // r0.add_node('P51111');
    // r0.add_node('ENSBTAP00000001972');
    // r0.add_node('ENSMODP00000004420');
    // r0.add_node('ENSOANP00000016906');
    // r0.add_node('XP_420822');
    // r0.add_node('Q66KL5');
    // r0.add_node('AN24');
    // r0.add_node('P51112');
    // r0.add_node('AN27');
    // r0.add_node('ENSCINP00000020100');
    // r0.add_node('ENSCINP00000020101');
    // r0.add_node('ENSCINP00000022904');
    // r0.add_node('ENSCINP00000022901');

    // r0.add_edge('AN24', 'P51112', 0.065);
    // r0.add_edge('AN24', 'O42269', 0.021);

    // r0.add_edge('AN16', 'P42859', 0.022);
    // r0.add_edge('AN16', 'P51111', 0.044);

    // r0.add_edge('AN12', 'P42858', 0.0);
    // r0.add_edge('AN12', 'ENSPTRP00000027313', 0.0);

    // r0.add_edge('AN11', 'AN12', 0.0);
    // r0.add_edge('AN11', 'ENSMMUP00000011008', 0.043);

    // r0.add_edge('AN10', 'AN11', 0.021);
    // r0.add_edge('AN10', 'AN16', 0.0);

    // r0.add_edge('AN9', 'AN10', 0.0);
    // r0.add_edge('AN9', 'ENSBTAP00000001972', 0.021);

    // r0.add_edge('AN8', 'AN9', 0.0);
    // r0.add_edge('AN8', 'ENSMODP00000004420', 0.0);

    // r0.add_edge('AN7', 'AN8', 0.0);
    // r0.add_edge('AN7', 'ENSOANP00000016906', 0.028);

    // r0.add_edge('AN6', 'AN7', 0.021);
    // r0.add_edge('AN6', 'XP_420822', 0.043);

    // r0.add_edge('AN5', 'AN6', 0.0);
    // r0.add_edge('AN5', 'Q66KL5', 0.028);

    // r0.add_edge('AN4', 'AN5', 0.065);
    // r0.add_edge('AN4', 'AN24', 0.044);

    // r0.add_edge('AN27', 'ENSCINP00000020100', 2.0);
    // r0.add_edge('AN27', 'ENSCINP00000020101', 2.0);
    // r0.add_edge('AN27', 'ENSCINP00000022904', 1.667);
    // r0.add_edge('AN27', 'ENSCINP00000022901', 1.667);

    // r0.add_edge('AN3', 'AN4', 0.118);
    // r0.add_edge('AN3', 'AN27', 0.822);

    // Set settable rendering properties.
    r0.use_animation = true;
    r0.box_width = 85;
    r0.box_height = 25;

    // Display.
    r0.display();

    ///
    /// Nonsense trials.
    /// 

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
