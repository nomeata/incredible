var graph = new joint.dia.Graph({
  loading: true
});

var paper = new joint.dia.Paper({
  el: $('#paper'),
  model: graph,
  width: 1000,
  height: 600,
  gridSize: 10,
  snapLinks: true,
  defaultLink: function (elementView, magnet) {
    return new joint.shapes.incredible.Link();
  },

  validateConnection: function (vs, ms, vt, mt, e, vl) {
    //console.log(vs,ms,vt,mt,e,vl);

    if (ms && mt && ms.getAttribute('direction') == mt.getAttribute('direction')) {
      return false;
    }

    return true;
  }
});

// zoom the viewport by 50%
paper.scale(1.5, 1.5);


function rescale_paper() {
  var paper_w = $("#paper").innerWidth() - 5;
  var paper_h = $("#paper").innerHeight() - 5;

  var bb = paper.getContentBBox();

  var w = Math.max(paper_w, bb.x + bb.width);
  var h = Math.max(paper_h, bb.y + bb.height);
  paper.setDimensions(w, h);
}

$(window).on('resize load', rescale_paper);

// Diagram setup
var task = examples.tasks.curry1;
var logic = examples.logics.conjAndImp;

function setupGraph(graph, logic, task) {
  var cells = [];
  // Fixed blocks for input and output
  $.each(task.assumptions || [], function (i, c) {
    var n = i + 1;
    var gate = new joint.shapes.incredible.Generic({
      position: {x: 120, y: 30 + 50 * i},
      assumption: n,
      task: task
    });
    cells.push(gate);
  });
  $.each(task.conclusions || [], function (i, c) {
    var n = i + 1;
    var gate = new joint.shapes.incredible.Generic({
      position: {x: 590, y: 30 + 50 * i},
      conclusion: n,
      task: task
    });
    cells.push(gate);
  });

  graph.resetCells(cells);

  setupPrototypeElements();

  rescale_paper();
}

function setupPrototypeElements() {
  var blockDescs = [];
  $.each(logic.rules, function (_, rule) {
    var blockDesc = ruleToBlockDesc(rule);
    blockDesc.isPrototype = true;
    blockDesc.data = {rule: rule};
    blockDescs.push(blockDesc)
  });
  var annBlockDesc = annotationToBlockDesc("P");
  annBlockDesc.isPrototype = true;
  annBlockDesc.data = {annotation: "P"};
  blockDescs.push(annBlockDesc);

  var container = $("#logic");
  container.empty();

  $.each(blockDescs, function (_, blockDesc) {
    var el = $('<div><svg xmlns="http://www.w3.org/2000/svg" width="300px" height="100px"></div>');
    container.append(el);
    var vel = V(el.find("svg").get(0));

    var g = V("<g/>");
    vel.append(g);
    renderBlockDescToSVG(g, blockDesc, false);
    g.scale(1.5);
    gBB = g.bbox(false);
    g.translate($(el).width()/2, -gBB.y + 5)

    $(el).height(gBB.height + 10);
    $(el).data('elementData', blockDesc.data);
    $(el).draggable({
      appendTo: "body",
      helper: "clone"
    });
  });
}

function isTrashArea(x, y) {
  return x < 0;
}

paper.on('cell:pointerdown', function (cellView, evt, x, y) {
  var cell = cellView.model;
  if (cell) {
    cell.set('originalPosition', cell.get('position'));
  }
});
paper.on('cell:pointerup', function (cellView, evt, x, y) {
  var cell = cellView.model;
  if (cell) {
    var trashCell = isTrashArea(x, y);
    if (trashCell) {
      if (cell.get('assumption') || cell.get('conclusion'))
        cell.set('position', cell.get('originalPosition'));
      else
        cell.remove();
    }
  }
});

paper.on('cell:pointerclick', function (cellView, evt, x, y) {
  var cell = cellView.model;
  if (cell.get('annotation')) {
    prop = window.prompt('Input proposition', cell.get('annotation'));
    if (prop) {
      cell.set('annotation', prop);
    }
  }
});

$.each(examples.tasks, function (name, l) {
  $("#taskselect").append(
    $("<option />").val(name).text(name)
  );
});
$.each(examples.graphs, function (name, l) {
  $("#proofselect").append(
    $("<option />").val(name).text(name)
  );
});

function with_graph_loading(func) {
  return function() {
    graph.set('loading', true);
    func.apply(this,arguments);
    graph.set('loading', false);
  }
}


function selectNoTask() {
  $("#taskselect").val("");
  $("#task").hide();
  $("#inferredrule").show();
  task = { };
  setupGraph(graph, logic, task);
  processGraph();
}

function selectLogic(name) {
  logic = examples.logics[name];
  setupPrototypeElements();
};

function selectTask(name) {
  task = examples.tasks[name];
  selectLogic(task.logic);

  $("#taskselect").val(name);
  $("#proofselect").val("");
  $("#assumptions").empty();
  $.each(task.assumptions || [], function (i, el) {
    $("#assumptions").append($("<div>").text(el));
  });
  $("#conclusions").empty();
  $.each(task.conclusions || [], function (i, el) {
    $("#conclusions").append($("<div>").text(el));
  });
  $("#inferredrule").hide();
  $("#task").show();
  setupGraph(graph, logic, task);
}

function selectProof(name) {
  proof = examples.graphs[name];
  selectTask(proof.task);
  graph.fromJSON(proof);

  // This is mostly for backwards compatibility with old stored graphs, and can
  // be removed eventually
  $.each(graph.getElements(), function (i, el) {
    if (el.get('prototypeElement')) {el.remove()};
  });
  processGraph();
}

$(function (){
  $("#taskselect").change(with_graph_loading(function () { if (this.value) selectTask(this.value); }));
  $("#proofselect").change(with_graph_loading (function () { if (this.value) selectProof(this.value); }));
  $("#freeproof").click(with_graph_loading (function () { selectNoTask(); }));

  selectTask('conjself');
  graph.set('loading', false);

  $("#showdialog").click(function(){
    $("#graph").val(JSON.stringify(graph.toJSON(),null,2));
    $("#proof").val(JSON.stringify(buildProof(graph)));
    $("#dialog").toggle();
  });
  $("#closedialog").click(function(){
    $("#dialog").hide()}
  );

  $("#inferredrule #inferredrulewrapper").draggable({
    appendTo: "body",
    helper: "clone"
    // helper: function ()  { return $("<span>Hi</span>") }
  });
});

// Don't wrap 'droppable' in $(..), or it won't work with Google Chrome. Why?
// Who knows...
$("#paper").droppable({
  drop: function (event, ui) {
    var data = ui.draggable.data('elementData');
    if (data) {
      var pos = paper.clientToLocalPoint({x: event.clientX, y: event.clientY});
      var elem = new joint.shapes.incredible.Generic(_.extend(data, {
        position: pos
      }));
      graph.addCell(elem);
    };
  }
});

graph.on('add remove change:annotation change:loading', function () {
  // Do not process the graph when loading is one, which happens during startup
  // and during batch changes.
  if (!graph.get('loading')) {
    processGraph();
  }
});
graph.on('change:source change:target', function (model, end) {
  var connection_state = model.get('source').id + model.get('source').port
    + model.get('target').id + model.get('target').port;
  var connection_state_old = model.get('connection_state');
  if (connection_state != connection_state_old) {
    model.set('connection_state', connection_state);
    if (!graph.get('loading')) {
      processGraph();
    }
  }
});

function processGraph() {
  $("#analysis").val();
  var proof = buildProof(graph);
  var timeBefore = performance.now();
  var analysis = incredibleLogic(logic, task, proof);
  var timeAfter = performance.now();

  $("#took").text("processing took " + (timeAfter - timeBefore).toFixed(1) + "ms");

  if (typeof analysis === 'string' || analysis instanceof String) {
    $("#analysis").val(analysis);
    $("#errors").text(analysis);
    $("#inferredrule svg").empty();
  } else {
    $("#analysis").val(JSON.stringify(analysis, null, 2));
    $("#errors").empty()

    // mock
    // analysis.rule = logic.rules[0];

    if ($("#inferredrule").is(':visible')) {
      if (analysis.rule) {
        // TODO: Draw the block here
        $("#inferredrule svg").each(function (n, el) {
          $(el).empty();
          var g = V("<g/>");
          var vel = V(el).append(g);
          var blockDesc = ruleToBlockDesc(analysis.rule);
          blockDesc.isPrototype = true;
          blockDesc.label = '☃';
          renderBlockDescToSVG(g, blockDesc, false);
          g.scale(1.5);
          gBB = g.bbox(false);
          g.translate($(el).width()/2, gBB.height/2 + 5)
          $(el).height(gBB.height + 10);
        });
      } else {
        $("#inferredrule svg").each(function (n, el) {
          $(el).empty();
          V(el).append(V("<text fill='black'/>").text("nothing"));
        });
      }
    }

    // Reset everything
    $.each(graph.getElements(), function (i, el) {
      el.set('brokenPorts',{});
    });
    $.each(graph.getLinks(), function (i, conn) {
      conn.attr({'.connection': {class: 'connection'}});
    });

    // We set this simly for all elements, even if only
    // the view for conclusion elements listens for it
    $.each(graph.getElements(), function (i, el) {
      el.set('qed', analysis.qed);
    });

    // Collect errors
    $.each(analysis.cycles, function (i, path) {
      $.each(path, function (i, connId) {
        var conn = graph.getCell(connId);
        // not very nice, see http://stackoverflow.com/questions/32010888
        conn.attr({'.connection': {class: 'connection error'}});
      });
    });
    $.each(analysis.escapedHypotheses, function (i, path) {
      $.each(path, function (i, connId) {
        var conn = graph.getCell(connId);
        // not very nice, see http://stackoverflow.com/questions/32010888
        conn.attr({'.connection': {class: 'connection error'}});
      });
    });

    $.each(analysis.unconnectedGoals, function (i, goal) {
      if (goal.block) {
        el = graph.getCell(goal.block);
	var bp = _.clone(el.get('brokenPorts'));
	bp[goal.port] = true;
	el.set('brokenPorts', bp);
      }
      if (goal.conclusion) {
        $.each(graph.getElements(), function (i, el) {
          if (el.get('conclusion') && el.get('conclusion') == goal.conclusion) {
            el.set('brokenPorts',{in:true});
          }
        });
      }
    });

    for (var connId in analysis.connectionLabels) {
      var lbl = analysis.connectionLabels[connId];
      var conn = graph.getCell(connId);
      if (lbl.propIn && lbl.propOut) {
        var symbol;
        if (lbl.type == "mismatch")   {symbol = "☠"}
        else if (lbl.type == "dunno") {symbol = "?"}
        else {console.log("Unknown connection label type")}
        conn.set('labels', [{
          position: .1,
          attrs: {
            text: {
              text: lbl.propIn
            }
          }
        },
          {
            position: .5,
            attrs: {
              text: {
                text: symbol
              }
            }
          },
          {
            position: .9,
            attrs: {
              text: {
                text: lbl.propOut
              }
            }
          }
        ]);
      } else {
        conn.set('labels', [{
          position: .5,
          attrs: {
            text: {
              text: lbl
            }
          }
        }]);
      }
    }
  }
}

function buildProof(graph) {
  var proof = {};

  proof.blocks = {};
  graph.getElements().map(
    function (e, i) {
      if (e.get('assumption') || e.get('conclusion') || e.get('prototypeElement')) {
        return;
      }
      proof.blocks[e.id] = {};
      var rule, annotation;
      if (rule = e.get('rule')) {
        proof.blocks[e.id].rule = rule.id;
      } else if (annotation = e.get('annotation')) {
        proof.blocks[e.id].annotation = annotation;
      } else {
        throw new Error("buildProof(): Unknown block type");
      }
    });

  proof.connections = {};
  graph.getLinks().map(
    function (l, i) {
      var con = {};
      con.from = makeConnEnd(graph, l.get('source'));
      con.to = makeConnEnd(graph, l.get('target'));
      proof.connections[l.id] = con;
    });
  return proof;
}

function makeConnEnd(graph, x) {
  var ret = {};
  var c = graph.getCell(x.id);
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
