////////////
////
//// bbop.render.phylo
////
//// Purpose: Extend the model to be handy for a (phylo)tree.
//// 
//// Taken name spaces:
////    bbop.render.phylo.*
////
//// TODO: see: http://raphaeljs.com/graffle.html
////
//// Required:
////    bbop.core
////    bbop.model
////
//////////


// Module and namespace checking.
//bbop.core.require('Raphael');
bbop.core.require('bbop', 'core');
bbop.core.require('bbop', 'model');
bbop.core.require('bbop', 'model', 'tree');
bbop.core.namespace('bbop', 'render', 'phylo');


// // Same same.
// bbop.model.tree.node = function(new_id){
//     bbop.model.node.call(this, new_id);
// };
// bbop.model.tree.node.prototype = new bbop.model.node;

// Start experimenting with this.
Raphael.fn.connection = function (obj1, obj2, line, bg) {
    if (obj1.line && obj1.from && obj1.to) {
        line = obj1;
        obj1 = line.from;
        obj2 = line.to;
    }
    var bb1 = obj1.getBBox();
    var bb2 = obj2.getBBox();
    var p = [{x: bb1.x + bb1.width / 2, y: bb1.y - 1},
             {x: bb1.x + bb1.width / 2, y: bb1.y + bb1.height + 1},
             {x: bb1.x - 1, y: bb1.y + bb1.height / 2},
             {x: bb1.x + bb1.width + 1, y: bb1.y + bb1.height / 2},
             {x: bb2.x + bb2.width / 2, y: bb2.y - 1},
             {x: bb2.x + bb2.width / 2, y: bb2.y + bb2.height + 1},
             {x: bb2.x - 1, y: bb2.y + bb2.height / 2},
             {x: bb2.x + bb2.width + 1, y: bb2.y + bb2.height / 2}];
    var d = {};
    var dis = [];
    for (var i = 0; i < 4; i++) {
        for (var j = 4; j < 8; j++) {
            var dx = Math.abs(p[i].x - p[j].x);
            var dy = Math.abs(p[i].y - p[j].y);
            if ((i == j - 4) ||
		(((i != 3 && j != 6) || p[i].x < p[j].x) &&
		 ((i != 2 && j != 7) || p[i].x > p[j].x) &&
		 ((i != 0 && j != 5) || p[i].y > p[j].y) &&
		 ((i != 1 && j != 4) || p[i].y < p[j].y))) {
                dis.push(dx + dy);
                d[dis[dis.length - 1]] = [i, j];
            }
        }
    }
    var res = null;
    if (dis.length == 0) {
        res = [0, 4];
    }else{
        res = d[Math.min.apply(Math, dis)];
    }
    var x1 = p[res[0]].x;
    var y1 = p[res[0]].y;
    var x4 = p[res[1]].x;
    var y4 = p[res[1]].y;
    var dx = Math.max(Math.abs(x1 - x4) / 2, 10);
    var dy = Math.max(Math.abs(y1 - y4) / 2, 10);
    var x2 = [x1, x1, x1 - dx, x1 + dx][res[0]].toFixed(3),
    y2 = [y1 - dy, y1 + dy, y1, y1][res[0]].toFixed(3),
    x3 = [0, 0, 0, 0, x4, x4, x4 - dx, x4 + dx][res[1]].toFixed(3),
    y3 = [0, 0, 0, 0, y1 + dy, y1 - dy, y4, y4][res[1]].toFixed(3);
    var path = ["M", x1.toFixed(3), y1.toFixed(3),
		"C", x2, y2, x3, y3, x4.toFixed(3), y4.toFixed(3)].join(",");
    if (line && line.line) {
        line.bg && line.bg.attr({path: path});
        line.line.attr({path: path});
    }else{
        var color = typeof line == "string" ? line : "#000";
        return {
            bg: bg && bg.split && this.path(path).attr({stroke: bg.split("|")[0], fill: "none", "stroke-width": bg.split("|")[1] || 3}),
            line: this.path(path).attr({stroke: color, fill: "none"}),
            from: obj1,
            to: obj2
        };
    }
};


// Example graph.
var n_a = new bbop.model.tree.node('a');
var n_b = new bbop.model.tree.node('b');
var n_c = new bbop.model.tree.node('c');
var n_d = new bbop.model.tree.node('d');
var n_e = new bbop.model.tree.node('e');
var n_f = new bbop.model.tree.node('f');
var e1 = new bbop.model.tree.edge(n_a, n_b, 1.1);
var e2 = new bbop.model.tree.edge(n_b, n_c, 2.2);
var e3 = new bbop.model.tree.edge(n_b, n_d, 3.3);
var e4 = new bbop.model.tree.edge(n_a, n_e, 4.4);
var e5 = new bbop.model.tree.edge(n_d, n_f, 5.5);
var t = new bbop.model.tree.graph();
t.add_node(n_a);
t.add_node(n_b);
t.add_node(n_c);
t.add_node(n_d);
t.add_node(n_e);
t.add_node(n_f);
t.add_edge(e1);
t.add_edge(e2);
t.add_edge(e3);
t.add_edge(e4);
t.add_edge(e5);

var fudge_x = 50;
var fudge_y = 50;
//var 
var layout = t.layout();
//var r = Raphael("test1", 640, 480);



// Render out.
window.onload = function () {

    // Dragging animation (color dimming).
    var dragger = function () {
        this.ox = this.attr("x");
        this.oy = this.attr("y");
        this.animate({"fill-opacity": .2}, 100);
    };

    // Movement animation (redo line).
    var move = function (dx, dy) {
        var att = {x: this.ox + dx, y: this.oy + dy};
        this.attr(att);
        for (var i = connections.length; i--;) {
            r.connection(connections[i]);
        }
        r.safari();
    };

    // Undrag animation.
    var up = function () {
        this.animate({"fill-opacity": 0}, 100);
    };

    var r = Raphael("test1", 640, 480);
    var connections = [];
    var shapes = [
	//r.ellipse(190, 100, 30, 20),
	r.rect(190, 100, 60, 40, 2),
        r.rect(290, 80, 60, 40, 2),
        r.rect(290, 180, 60, 40, 2),
        //r.ellipse(450, 100, 20, 20),
        r.rect(450, 100, 60, 40, 2),
        r.rect(450, 180, 60, 40, 2),
        r.rect(190, 180, .1, .1, 0)
    ];
    
    // Shape definition.
    for (var i = 0, ii = shapes.length; i < ii; i++) {
        var color = Raphael.getColor();
        shapes[i].attr({fill: color,
			stroke: color,
			"fill-opacity": .25,
			"stroke-width": 2,
			cursor: "move"});
        shapes[i].drag(move, dragger, up);
    }

    // 
    connections.push(r.connection(shapes[0], shapes[1], "#00f"));
    connections.push(r.connection(shapes[1], shapes[2], "#0f0", "#fff|5"));
    connections.push(r.connection(shapes[1], shapes[3], "#f00", "#fff"));
    connections.push(r.connection(shapes[4], shapes[2], "#f00", "#fff"));
    connections.push(r.connection(shapes[5], shapes[0], "#f0f", "#fff"));
};
