var graph = new joint.dia.Graph();

var paper = new joint.dia.Paper({
    el: $('#paper'),
    model: graph,
    width: 1000,
    height: 600,
    gridSize: 1,
    snapLinks: true,
    defaultLink: function (elementView, magnet) {
        e = new joint.shapes.logic.Wire();
        e.set('router', {name: 'manhattan'});
        return e;
    },

    validateMagnet: function (v, m) {
        return !v.model.get('prototypeElement');
    },

    validateConnection: function (vs, ms, vt, mt, e, vl) {
        //console.log(vs,ms,vt,mt,e,vl);

        if (ms && mt && ms.getAttribute('direction') == mt.getAttribute('direction')) {
            return false;
        }

        if (e === 'target') {
            if (vt.model.get('prototypeElement')) return false;

            /* Disabled: It may be convenient to temporarily attach
             * two links to a port

            // check whether the port is being already used
            var portUsed = _.find(this.model.getLinks(), function (link) {

                return (link.id !== vl.model.id &&
                link.get('target').id === vt.model.id &&
                link.get('target').port === mt.getAttribute('port'));
            });
            return !portUsed;
            */
            return true;
        } else { // e === 'source'
            return true;
        }
    }
});

// zoom the viewport by 50%
paper.scale(1.5, 1.5);


function rescale_paper () {
    var paper_w = $("#paper").innerWidth()-5;
    var paper_h = $("#paper").innerHeight()-5;

    V($("#vertical-separator").get(0)).attr({y2:0});
    var bb = paper.getContentBBox();

    var w = Math.max(paper_w, bb.x + bb.width);
    var h = Math.max(paper_h, bb.y + bb.height);
    paper.setDimensions(w,h);
    V($("#vertical-separator").get(0)).attr({y2:h});
}

$(window).on('resize', rescale_paper);

// Diagram setup
var task = examples.tasks.curry1;
var logic = examples.logics.conjAndImp;

var conclusionModels = [];

function setupGraph(graph, logic, task) {
    var cells = [];
    // Fixed blocks for input and output
    $.each(task.assumptions, function (i,c) {
        var n = i+1;
        var gate = new joint.shapes.incredible.Assumption({
                    position: {x: 230, y: 30 + 50 * i},
                    attrs: { text: {text: c}},
                    assumption: n,
                    });
        cells.push(gate);
    });
    $.each(task.conclusions, function (i,c) {
        var n = i+1;
        var gate = new joint.shapes.incredible.Conclusion({
                    position: {x: 550, y: 300 + 50 * i},
                    attrs: { text: {text: c}},
                    conclusion: n,
                    });
        cells.push(gate);
        conclusionModels[i] = gate;
    });

    // "Prototype blocks" for each element
    $.each(logic.rules, function(i,rule) {
        var n = i+1;
        var baseClass;
        if (shapes[rule.id]){
            baseClass = shapes[rule.id];
        } else {
            baseClass = joint.shapes.incredible.Generic;
        }
        // Is this overly complicated?
        elemClass = baseClass.extend({
            defaults: joint.util.deepSupplement(
                { rule: rule },
                baseClass.prototype.defaults),
        });
        var elem = new elemClass({
                        originalPosition: {x: 50, y: 25 + 50 * i},
                        position: {x: 50, y: 25 + 50 * i},
                        prototypeElement: true,
                        });
        cells.push(elem);
    });

    graph.resetCells(cells);

    // Vertical line
    var line = V('line', { id: "vertical-separator", x1: 200, y1: 5, x2: 200, y2: 400, stroke: 'grey' });
    V(paper.viewport).append(line);
}

paper.on('cell:pointerdown', function(cellView, evt, x, y) {
    var cell = cellView.model;
    if (cell && cell.get('prototypeElement')) {
        cell.toFront();
    }
});
paper.on('cell:pointerup', function(cellView, evt, x, y) {
    var cell = cellView.model;
    if (cell && cell.get('prototypeElement')) {
        // Add a new element
        var newElem = cell.clone();
        newElem.set('prototypeElement', false);
        graph.addCell(newElem);

        // Reset prototype cell
        cell.set('position',newElem.get('originalPosition'));
    }
});

$.each(examples.tasks, function(name,l) {
    $("#taskselect").append(
        $("<option />").val(name).text(name)
    );
});

$("#taskselect").change(function () { if (this.value) selectTask(this.value); });

function selectTask(name) {
    task = examples.tasks[name];
    logic = examples.logics[task.logic];
    $("#taskselect").val(name);
    $("#assumptions").empty();
    $.each(task.assumptions, function (i, el) {
        $("#assumptions").append($("<div>").text(el));
    });
    $("#conclusions").empty();
    $.each(task.conclusions, function (i, el) {
        $("#conclusions").append($("<div>").text(el));
    });
    setupGraph(graph, logic, task);
    processGraph();
}

$(function () {
    selectTask('conjself');
    rescale_paper();
});

$("#update").click(processGraph);
graph.on('add remove ', function() { processGraph(); });
graph.on('change:source change:target', function(model, end) {
    var connection_state = model.get('source').id + model.get('source').port
                         + model.get('target').id + model.get('target').port;
    var connection_state_old = model.get('connection_state');
    if (connection_state != connection_state_old) {
        model.set('connection_state', connection_state);
        processGraph();
    }
});

function processGraph() {
    $("#analysis").val();
    var proof = buildProof(graph);
    var timeBefore = performance.now();
    var analysis = incredibleLogic(logic,task,proof);
    var timeAfter = performance.now();

    $("#took").text("processing took " + (timeAfter-timeBefore).toFixed(1) + "ms");

    if (typeof analysis === 'string' || analysis instanceof String) {
        $("#analysis").val(analysis);
    } else {
        $("#analysis").val(JSON.stringify(analysis, null, 2));

        // Reset everything
        $.each(graph.getElements(), function (i, el) {
            var rule = el.get('rule');
            if (rule) {
                $.each(rule.ports, function (p, c) {
                    el.attr('.port'+p+'>.port-body', {fill:'#777'});
                });
            }
            if (el.get('conclusion')) {
                el.attr('circle', {fill:'#777'});
            }
        });
        $.each(graph.getLinks(), function (i, conn) {
            conn.attr({'.connection': { class: 'connection' }});
        });

        if (analysis.qed) {
            conclusionModels.map(function (c) {
                c.attr({'rect': {'class': 'body qed'}});
            });
        } else {
            conclusionModels.map(function (c) {
                c.attr({'rect': {'class': 'body'}});
            });
        }

        // Collect errors
        $.each(analysis.cycles, function (i,path) {
            $.each(path, function (i,connId) {
                conn = graph.getCell(connId);
                // not very nice, see http://stackoverflow.com/questions/32010888
                conn.attr({'.connection': { class: 'connection error' }});
            });
        });
        $.each(analysis.escapedHypotheses, function (i,path) {
            $.each(path, function (i,connId) {
                conn = graph.getCell(connId);
                // not very nice, see http://stackoverflow.com/questions/32010888
                conn.attr({'.connection': { class: 'connection error' }});
            });
        });

        $.each(analysis.unconnectedGoals, function (i,goal) {
            if (goal.block) {
                el = graph.getCell(goal.block);
                el.attr('.port'+goal.port+'>.port-body', {fill:'#F00'});
            }
            if (goal.conclusion) {
                conclusionModels[goal.conclusion-1].attr('circle', {fill:'#F00'});
            }
        });

        for (connId in analysis.connectionLabels) {
            lbl = analysis.connectionLabels[connId];
            conn = graph.getCell(connId);
            if (lbl.propIn && lbl.propOut) {
                conn.set('labels', [{
                    position: .1,
                    attrs: {
                        text: {
                            text: lbl.propIn,
                        }
                    }
                },
                {
                    position: .5,
                    attrs: {
                        text: {
                            text: '☠'
                        }
                    }
                },
                {
                    position: .9,
                    attrs: {
                        text: {
                            text: lbl.propOut,
                        }
                    }
                }
            ]);
            } else {
                conn.set('labels', [{
                    position: .5,
                    attrs: {
                        text: {
                            text: lbl,
                        }
                    }
                }]);
        }
    }
    }
};

function buildProof(graph) {
    var proof = {}

    proof.blocks = {}
    graph.getElements().map(
        function (e,i) {
            if (e.get('assumption') || e.get('conclusion') || e.get('prototypeElement')) {
                return;
            }
            proof.blocks[e.id] = {};
            proof.blocks[e.id]['rule'] = e.get('rule').id;
        });

    proof.connections = {}
    graph.getLinks().map(
        function(l,i) {
            con = {};
            con.from = makeConnEnd(graph, l.get('source'));
            con.to = makeConnEnd(graph, l.get('target'));
            proof.connections[l.id] = con;
        });
    return proof;
}

function makeConnEnd(graph, x) {
    ret = {};
    c = graph.getCell(x.id);
    if (!c) {
        return ret;
    }
    if (c.get('assumption')) {
        ret.assumption = c.get('assumption');
    } else if (c.get('conclusion')) {
        ret.conclusion = c.get('conclusion');
    } else {
        ret.block = x.id;
        ret.port = x.port;
    }
    return ret;
}
