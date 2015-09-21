// Some global variables
var mode = {}; // Where does the current task come from?
var task; // The current task
var logic; // The current logic

sessions.custom = {tasks: []};

// What tasks of the session were solved
var session_solved = {};
var session_saved = {};


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


function setupGraph(graph, logic, task) {
  var cells = [];
  // Fixed blocks for input and output
  $.each(task.assumptions || [], function (i, c) {
    var n = i + 1;
    var gate = new joint.shapes.incredible.Generic({
      position: {x: 120, y: 30 + 50 * i},
      assumption: n,
      task: task,
      number: cells.length + 1
    });
    cells.push(gate);
  });
  $.each(task.conclusions || [], function (i, c) {
    var n = i + 1;
    var gate = new joint.shapes.incredible.Generic({
      position: {x: 590, y: 30 + 50 * i},
      conclusion: n,
      task: task,
      number: cells.length + 1
    });
    cells.push(gate);
  });

  graph.resetCells(cells);

  setupPrototypeElements();

  rescale_paper();
}

function renderBlockDescToDraggable(blockDesc, container) {
  var el = $('<div><svg xmlns="http://www.w3.org/2000/svg"></div>');
  container.append(el);
  var vel = V(el.find("svg").get(0));

  var g = V("<g/>");
  vel.append(g);
  renderBlockDescToSVG(g, blockDesc, false);
  g.scale(1.5);
  gBB = g.bbox(false);
  g.translate($(el).width()/2, -gBB.y + 5)

  vel.attr({'width': container.width(), 'height': gBB.height + 10 });
  $(el).data('elementData', blockDesc.data);
  $(el).draggable({
    appendTo: "body",
    helper: "clone"
  });
}

function setupPrototypeElements() {
  var container = $("#logic");
  container.empty();
  $.each(logic.rules, function (_, rule) {
    var blockDesc = ruleToBlockDesc(rule);
    blockDesc.isPrototype = true;
    blockDesc.canRemove = false;
    blockDesc.data = {rule: rule};
    renderBlockDescToDraggable(blockDesc, container);
  });

  var container = $("#helpers");
  container.empty();
  var annBlockDesc = annotationToBlockDesc("P");
  annBlockDesc.isPrototype = true;
  annBlockDesc.canRemove = false;
  annBlockDesc.data = {annotation: "P"};
  renderBlockDescToDraggable(annBlockDesc, container);
}

paper.on('cell:pointerdown', function (cellView, evt, x, y) {
  var cell = cellView.model;

  if (evt.shiftKey) { return; }

  // Check if this was a click on a delete element
  // This assumes that all visible elements of the delete SVG are direct childs
  // of a <g> element with event="remove" set
  var targetParentEvent = evt.target.parentNode.getAttribute('event');
  if (targetParentEvent && targetParentEvent == "remove" ) {
    cell.remove();
    return;
  }
});

paper.on('blank:pointerclick', function (evt, x, y) {
  if (evt.shiftKey) {
    // ignore
  } else {
    $.each(graph.getElements(), function (i, el) {
      el.set('selected', false);
    });
  }
});

paper.on('cell:pointerclick', function (cellView, evt, x, y) {
  var cell = cellView.model;

  if (evt.shiftKey) {
    cell.set('selected', ! cell.get('selected'));
  } else {
    // Deselect everything
    $.each(graph.getElements(), function (i, el) {
      el.set('selected', false);
    });

    if (cell.get('annotation')) {
      var done = false;
      var prmpt = 'Input proposition';
      var val = cell.get('annotation');
      while (!done) {
        val = window.prompt(prmpt, val);
        if (val) {
          var prettyPrinted = incredibleFormatTerm(val);
          if (prettyPrinted) {
            done = true;
            cell.set('annotation', prettyPrinted);
          } else {
            prmpt = 'Could not parse, please try again:';
          }
        } else {
          done = true;
        }
      }
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
    processGraph();
  }
}

function saveTask() {
  if (mode.hasOwnProperty('session')) {
    session_saved[mode.session] = session_saved[mode.session] || {};
    session_saved[mode.session][mode.task] = _.omit(graph.toJSON(), 'loading');
    saveSession()
  }
}

function saveSession() {
  localStorage["incredible-session"] = JSON.stringify({
    saved: session_saved,
    solved: session_solved,
    custom: sessions.custom
  });
}

function loadSession() {
  if (localStorage["incredible-session"]) {
    var stored = JSON.parse(localStorage["incredible-session"]);
    session_saved  = stored.saved || {};
    session_solved = stored.solved || {};
    sessions.custom = stored.custom || {tasks: []};
  }
}

function selectNoTask() {
  $("#taskselect").val("");
  $("#task").hide();
  $("#inferredrule").show();
  saveTask();
  task = { };
  setupGraph(graph, logic, task);
  processGraph();
}

function selectLogic(name, visible) {
  logic = _.clone(examples.logics[name || 'predicate']);

  if (visible) {
    logic.rules = _.filter(logic.rules, function (r) {
      return _.includes(visible, r.id);
    });
  }
  // Normalize the input here
  $.each(logic.rules, function (_,r) {
    $.each(r.ports, function (_,p) {
      p.proposition = incredibleFormatTerm(p.proposition)
    });
  });

  setupPrototypeElements();
};

function selectNamedTask(name) {
  saveTask();
  selectLogic(examples.tasks[name].logic);
  loadTask(examples.tasks[name]);
  $("#taskselect").val(name);
}

function loadTask(thisTask) {
  task = thisTask;

  // Normalize the input here
  task.assumptions = (task.assumptions || []).map(incredibleFormatTerm);
  task.conclusions = (task.conclusions || []).map(incredibleFormatTerm);

  $("#proofselect").val("");

  $("#taskwrap")
    .empty()
    .append(taskToHTML(task))
    .show();
  $("#inferredrule").hide();
  setupGraph(graph, logic, task);
}


function selectSessionTask(evt) {
  saveTask();

  mode = {
    session: $(evt.currentTarget).data('session'),
    task: $(evt.currentTarget).data('task')
  }
  var session = sessions[mode.session];
  var thisTask = session.tasks[mode.task];

  selectLogic(session.logic, session["visible-rules"]);

  setupPrototypeElements();

  loadTask(thisTask);
  var saved_graph = (session_saved[mode.session]||{})[mode.task];
  if (saved_graph) {
    graph.fromJSON(saved_graph);
  }
  $("#taskdialog").hide();
}


function selectProof(name) {
  proof = examples.graphs[name];
  proof.loading = true;

  if (proof.task) {
    selectNamedTask(proof.task);
  } else if (proof.logic) {
    selectLogic(proof.logic);
    selectNoTask();
  } else {
    throw new Error("selectProof: Neither task nor logic: " + name);
  }

  graph.fromJSON(proof);
  // This is mostly for backwards compatibility with old stored graphs, and can
  // be removed eventually
  $.each(graph.getElements(), function (i, el) {
    if (el.get('prototypeElement')) {el.remove()};
  });
  proof.set('loading', false);
}

function taskToHTML(task) {
  d1 = $("<ul class='assumptions'>");
  $.each(task.assumptions || [], function (i, el) {
    d1.append($("<li>").text(incredibleFormatTerm(el)));
  });
  d2 = $("<ul class='conclusions'>");
  $.each(task.conclusions || [], function (i, el) {
    d2.append($("<li>").text(incredibleFormatTerm(el)));
  });
 return $("<div class='inferencerule'>").append(d1, $("<hr/>"), d2);
}

$(function () {
  $("#switchtask").on('click', function () {
    saveTask(); // to update session_saved
    showTaskSelection();
  });
});

function setupTaskSelection() {
  $.each(sessions, function (i,session) {
    $("<h3>").text(session.name).appendTo("#sessiontasks");
    var container = $("<div>").addClass("tasklist").appendTo("#sessiontasks");
    $.each(session.tasks, function (j,thisTask) {
      taskToHTML(thisTask)
        .data({session: i, task: j})
        .addClass("sessiontask-" + i + "-" + j)
        .on('click', with_graph_loading(selectSessionTask))
        .appendTo(container)
    });
  });

  $.each(sessions.custom.tasks, function (j,thisTask) {
    taskToHTML(thisTask)
      .data({session: 'custom', task: j})
      .addClass("sessiontask-" + 'custom' + "-" + j)
      .on('click', with_graph_loading(selectSessionTask))
      .insertBefore("#customtask")
  });

  $("#customtask #addcustomtask").on('click', function (){
    var thisTask = taskFromText($("#customtask textarea").val());
    if (thisTask) {
      var j = sessions.custom.tasks.length;
      sessions.custom.tasks.push(thisTask);
      taskToHTML(thisTask)
        .data({session: 'custom', task: j})
        .addClass("sessiontask-" + 'custom' + "-" + j)
        .on('click', with_graph_loading(selectSessionTask))
        .insertBefore("#customtask");
      saveSession();
    } else {
      alert('Sorry, could not understand this task');
    }
  });
}

function taskFromText(text) {
  var lines = text.match(/[^\r\n]+/g);
  var task = {assumptions: [], conclusions: []};
  var now = 'assumptions';
  var ok = true;
  $.each(lines, function (i, l) {
    if (l.match(/^[–─–_-]+$/)){
      now = 'conclusions';
    } else {
      var prop = incredibleFormatTerm(l);
      if (prop) {
        task[now].push(l);
      } else {
        ok = false;
      }
    }
  });
  if (ok && now == 'conclusions') {
    return task;
  } else {
    return
  }
}

function showTaskSelection() {
  $.each(session_solved, function (i, solved) {
    $.each(solved, function (j,_) {
      $(".sessiontask-" + i + "-" + j).addClass('solved');
    });
  });
  $.each(session_saved, function (i, attempted) {
    $.each(attempted, function (j,_) {
      $(".sessiontask-" + i + "-" + j).addClass('attempted');
    });
  });

  $("#taskdialog").show();
}

function blockNumberMap() {
  var numberMap = {};
  $.each(graph.getElements(), function (i, el) {
    if (el.get('number')) {
      numberMap[el.get('number')] = el
    }
  });
  return numberMap;
}

function nextFreeBlockNumber() {
  var numberMap = blockNumberMap();
  var n=1;
  while (numberMap[n]) {n+=1};
  return n;
}

$(function (){
  $("#taskselect").change(with_graph_loading(function () { if (this.value) selectNamedTask(this.value); }));
  $("#proofselect").change(with_graph_loading (function () { if (this.value) selectProof(this.value); }));
  $("#freeproof").click(with_graph_loading (function () { selectNoTask(); }));

  $("#showdialog").click(function(){
    $("#graph").val(JSON.stringify(graph.toJSON(),null,2));
    $("#proof").val(JSON.stringify(buildProof(graph)));
    $("#dialog").toggle();
  });
  $("#closedialog").click(function(){
    $("#dialog").hide()}
  );

  $("#showhelp").click(function(){
    $("#help").toggle();
  });
  $("#closehelp").click(function(){
    $("#help").hide()}
  );

  $("#reset").click(function(){
    if (window.confirm("Are you sure you want to remove all saved data (proofs, custom tasks, custom blocks)?")) {
      $(window).off("unload"); /* Otherwise we’d just save it again */
      localStorage.removeItem("incredible-session");
      window.location.reload(false);
    }
  });

  $(window).on('unload', function () {
    saveTask();
    saveSession();
  });

  $("#inferredrule #inferredrulewrapper").draggable({
    appendTo: "body",
    helper: "clone"
    // helper: function ()  { return $("<span>Hi</span>") }
  });

  $("#paper").droppable({
    drop: function (event, ui) {
      var data = ui.draggable.data('elementData');
      if (data) {
        var pos = paper.clientToLocalPoint({x: event.clientX, y: event.clientY});
        var elem = new joint.shapes.incredible.Generic(_.extend(data, {
          position: g.point(pos.x, pos.y).snapToGrid(paper.options.gridSize),
          number: nextFreeBlockNumber()
        }));
        graph.addCell(elem);
      };
    }
  });

  loadSession();
  setupTaskSelection();
  showTaskSelection();
  $("#loading").fadeOut({duration: 500});
});

paper.on('element:schieblehre',function(cellView, direction, dx, dy) {
  var basewidth0 = cellView.model.get('schieblehrebasewidth');
  if (basewidth0 === undefined) { basewidth0 = 40; }
  var basewidth1 = basewidth0;
  if (direction == "resize-left") {
    basewidth1 -= dx;
  } else if (direction == "resize-right") {
    basewidth1 += dx;
  } else {
    throw Error("element:schieblehre: Unknown direction " + direction)
  }
  cellView.model.set('schieblehrebasewidth', basewidth1);
  var width0 = cellView.model.get('schieblehrewidth');
  if (width0 === undefined) { width0 = 40; }
  var width1 = Math.max(0, g.snapToGrid(basewidth1, paper.options.gridSize));
  if (width0 != width1) {
    cellView.model.set('schieblehrewidth', width1);
    // Move center accordingly
    if (direction == "resize-left") {
      cellView.model.translate(width0 - width1, 0, {ui: true});
    }
  }
});


graph.on('change:position', function (model, pos1, options) {
  if (options.derivedMove) { return; }

  if (model.get('selected')) {
    var dx = options.tx;
    var dy = options.ty;
    if (dx == 0 && dy == 0) { return; }

    $.each(graph.getElements(), function (i, el) {
      if (el.get('selected') && el != model) {
        el.translate(dx,dy, { derivedMove : true })
      }
    });
  }
})

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

    if (mode.hasOwnProperty('session')) {
      if (analysis.qed) {
        session_solved[mode.session] = session_solved[mode.session] || {};
        session_solved[mode.session][mode.task] = true;
      }
    }

    // mock
    // analysis.rule = logic.rules[0];

    if ($("#inferredrule").is(':visible')) {
      if (analysis.rule) {
        $("#inferredrule svg").each(function (n, el) {
          $(el).empty();
          var g = V("<g/>");
          var vel = V(el).append(g);
          var blockDesc = ruleToBlockDesc(analysis.rule);
          blockDesc.canRemove = false;
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
      if (lbl.type == "mismatch" || lbl.type == "dunno") {
        var symbol;
        if (lbl.type == "mismatch")   {symbol = "☠"}
        else if (lbl.type == "dunno") {symbol = "?"}
        else {console.log("Unknown connection label type")}

        // not very nice, see http://stackoverflow.com/questions/32010888
        conn.attr({'.connection': {class: 'connection error'}});

        if (isReversed(conn)) {
          f = function (pos) {return 1-pos};
        } else {
          f = function (pos) {return pos};
        }

        conn.set('labels', [{
          position: f(.1),
          attrs: {
            text: {
              text: lbl.propIn
            }
          }
        },
          {
            position: f(.5),
            attrs: {
              text: {
                text: symbol
              }
            }
          },
          {
            position: f(.9),
            attrs: {
              text: {
                text: lbl.propOut
              }
            }
          }
        ]);
      } else if (lbl.type == "ok") {
        conn.set('labels', [{
          position: .5,
          attrs: {
            text: {
              text: lbl.prop
            }
          }
        }]);
      } else if (lbl.type == "unconnected") {
        conn.set('labels', []);
      } else {
        throw new Error("processGraph(): Unknown connection label type");
      }
    }
  }
}

function isReversed(conn) {
  // A connection is reversed if its source is an "in" magnet, or the target an
  // "out" magnet.
  var e = conn.get('source')
  if (e.id) {
    var el = graph.getCell(e.id);
    if (el.get('conclusion')) {
      return true;
    }
    if (el.get('annotation')) {
      if (e.port == "in") {
        return true;
      }
    }
    var rule;
    if (rule = el.get('rule')) {
      if (rule.ports[e.port].type == "assumption") {
        return true;
      }
    }
  }
  var e = conn.get('target')
  if (e.id) {
    var el = graph.getCell(e.id);
    if (el.get('assumption')) {
      return true;
    }
    if (el.get('annotation')) {
      if (e.port == "out") {
        return true;
      }
    }
    var rule;
    if (rule = el.get('rule')) {
      if (rule.ports[e.port].type == "conclusion") {
        return true;
      }
      if (rule.ports[e.port].type == "local hypothesis") {
        return true;
      }
    }
  }
  return false;
}

function buildProof(graph) {
  var proof = {};

  proof.blocks = {};
  graph.getElements().map(
    function (e, i) {
      if (e.get('assumption') || e.get('conclusion') || e.get('prototypeElement')) {
        return;
      }
      block = {}
      var rule, annotation;
      if (rule = e.get('rule')) {
        block.rule = rule.id;
      } else if (annotation = e.get('annotation')) {
        block.annotation = annotation;
      } else {
        throw new Error("buildProof(): Unknown block type");
      }
      block.number = e.get('number');
      proof.blocks[e.id] = block;
    });

  proof.connections = {};
  graph.getLinks().map(
    function (l, i) {
      var con = {};
      if (isReversed(l)){
        con.to =   makeConnEnd(graph, l.get('source'));
        con.from = makeConnEnd(graph, l.get('target'));
      } else {
        con.from = makeConnEnd(graph, l.get('source'));
        con.to =   makeConnEnd(graph, l.get('target'));
      }
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
