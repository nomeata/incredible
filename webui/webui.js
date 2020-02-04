// Some global variables
var task = null; // The current task
var logicName = null; // The name of the current logic, important for custom blocks etc.
var ruleFilter = constant_true;


function constant_true (r) {
  return true;
}

function current_logic_rules() {
  return logicName == null ? [] : _.filter(logics[logicName].rules, ruleFilter).concat();
}
function current_custom_rules() {
  return logicName == null ? [] : (custom_rules[logicName] || []);
}
function current_logic() {
  return {
    rules: current_logic_rules().concat(current_custom_rules())
  };
}


var graph = new joint.dia.Graph({
  loading: true
});
var paper = create_paper();

var undoList = [];
var currentState = -1;

function resetUndo() {
  if (!hasTask()) {
    undoList = [];
    currentState = -1;
  } else {
    var g = graph.toJSON();
    undoList = [{graph: g}];
    currentState = 0;
  }
}

function saveUndo() {
  if (currentState < 0) return;

  currentState += 1;
  var g = graph.toJSON();
  undoList[currentState] = {graph: g};
  undoList.length = currentState+1;
}

function applyUndoState(idx) {
  if (currentState < 0) return;

  var state = undoList[idx];
  if (state) {
    graph.fromJSON(state.graph);
    currentState = idx;
    processGraph();
  }
}

function setBlockCountBar(name, number) {
    $("#" + name + " span.number").text(number);
    // Up to 80% is ok...
    var width = Math.round(80 * (2/(1+Math.exp(-0.1 * number)) - 1));
    $("#" + name + " span.bar").css('width', width + "%");
}

function setupGraph(graph, task) {
  paper.setOrigin(0,0);
  // Lets preserve the scale; why not.
  // paper.scale(1.0);

  var cells = [];

  var scale = V(paper.viewport).scale();
  var w = $("#paper").innerWidth() / scale.sx;
  var h = $("#paper").innerHeight() / scale.sy;

  var ac = task.assumptions.length;
  var cc = task.conclusions.length;

  $("#blockcount").show();
  if (task["min-blocks"]) {
    $("#min-blockcount").show();
    setBlockCountBar('min-blockcount',task["min-blocks"]);
  } else {
    $("#min-blockcount").hide();
  }

  // Fixed blocks for assumptions and conclusions
  $.each(task.assumptions || [], function (i, c) {
    var n = i + 1;
    var gate = new joint.shapes.incredible.Generic({
      position: {x: 50, y: h / 2 - (ac-1)*25 + i*50},
      assumption: n,
      task: task,
      number: cells.length + 1
    });
    cells.push(gate);
  });
  $.each(task.conclusions || [], function (i, c) {
    var n = i + 1;
    var gate = new joint.shapes.incredible.Generic({
      position: {x: w-50, y: h / 2 - (cc-1)*25 + i*50},
      conclusion: n,
      task: task,
      number: cells.length + 1
    });
    cells.push(gate);
  });

  graph.resetCells(cells);

  setupPrototypeElements();
  processGraph();

}

function renderBlockDescToDraggable(blockDesc, container) {
  var el = $('<div><svg xmlns="http://www.w3.org/2000/svg"></div>');
  container.append(el);
  var vel = V(el.find("svg").get(0));

  var g = V("<g/>");
  vel.append(g);
  BlockDescRenderer(g, blockDesc, false).renderToSVG();
  gBB = g.bbox(false);
  var width = 2*Math.max(-gBB.x, gBB.width+gBB.x);
  g.translate(width/2, -gBB.y + 5);

  vel.attr({'width': width, 'height': gBB.height + 10 });
  $(el).data('elementData', blockDesc.data);
  $(el).draggable({
    appendTo: "body",
    helper: "clone"
  });
  return el;
}

// Do not really delete it, we might need it in proofs.
function delete_custom_block(id) {
  $.each(current_custom_rules(), function (_, rule) {
    if (rule.id == id) {
      rule.deleted = true;
    }
  });
  setupPrototypeElements();
}


function setupPrototypeElements() {
  var logic_container = $("#logic");
  logic_container.empty();
  $.each(current_logic_rules(), function (_, rule) {
    var blockDesc = ruleToBlockDesc(rule);
    blockDesc.isPrototype = true;
    blockDesc.canRemove = false;
    blockDesc.data = {rule: rule};
    renderBlockDescToDraggable(blockDesc, logic_container);
  });

  var custom_container = $("#custom");
  custom_container.empty();
  $.each(current_custom_rules(), function (_, rule) {
    if (!rule.deleted) {
      var blockDesc = ruleToBlockDesc(rule);
      blockDesc.isPrototype = true;
      blockDesc.canRemove = true;
      blockDesc.data = {rule: rule};
      el = renderBlockDescToDraggable(blockDesc, custom_container);
      $(el).find(".tool-remove").on('click', function () {
        delete_custom_block(rule.id);
      });
    }
  });

  var helpers_container = $("#helpers");
  helpers_container.empty();
  var annBlockDesc = annotationToBlockDesc("P");
  annBlockDesc.isPrototype = true;
  annBlockDesc.canRemove = false;
  annBlockDesc.data = {annotation: "P"};
  renderBlockDescToDraggable(annBlockDesc, helpers_container);
}

function with_graph_loading(func) {
  return function() {
    // Doesn't actually work
    // $("#loading").show();
    graph.set('loading', true);
    func.apply(this,arguments);
    graph.set('loading', false);
    processGraph();
    // $("#loading").hide();
    resetUndo();
  };
}

function loadTask(thisTask, taskSaved, thisLogicName, visibleRules) {
  task = thisTask;
  logicName = thisLogicName || 'predicate';

  // Normalize the input here
  $.each(logics[logicName].rules, function (_,r) {
    $.each(r.ports, function (_,p) {
      p.proposition = incredibleFormatTerm(p.proposition);
    });
  });

  if (visibleRules) {
    ruleFilter= function (r) {
      return _.includes(visibleRules, r.id);
    };
  } else {
    ruleFilter = constant_true;
  }

  $("#proofselect").val("");

  $("#taskwrap")
    .empty()
    .append(taskToHTML(task))
    .show();
  $("#inferredrule").hide();

  $("#paper-toolbar").show();
  $("#leftpane").show();

  setupGraph(graph, task);
  setupPrototypeElements();

  if (taskSaved) graph.fromJSON(taskSaved);
}

function unloadTask() {
  task = null;
  logicName = null;
  ruleFilter = constant_true;

  graph.set('loading', true);
  graph.clear();

  undoList = [];
  currentState = -1;

  $("#taskwrap").empty();

  $("#blockcount").hide();
  $("#min-blockcount").hide();

  $("#logic").empty();
  $("#custom").empty();
  $("#helpers").empty();
  
  $("#paper-toolbar").hide();
  $("#leftpane").hide();
}

function hasTask() {
  return task != null;
}

function blockNumberMap() {
  var numberMap = {};
  $.each(graph.getElements(), function (i, el) {
    if (el.get('number')) {
      numberMap[el.get('number')] = el;
    }
  });
  return numberMap;
}

function nextFreeBlockNumber() {
  var numberMap = blockNumberMap();
  var n=1;
  while (numberMap[n]) {n+=1;}
  return n;
}

$(function (){
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
        saveUndo();
      }
    }
  });
});

$(function (){
  $("#showhelp").click(function(){
    $("#help").toggle();
  });
  $("#closehelp").click(function(){
    $("#help").hide();
  });

  $("#closedialog").click(function(){
    $("#dialog").hide();
  });

  $("#reset").click(function(){
    if (window.confirm(i18n.t("confirm-reset"))) {
      reset_everything();
    }
  });

  $("#resettask").on('click', function () {
    if (!hasTask()) return;

    setupGraph(graph, task);
    resetUndo(); // Is this what we want?
  });

  $("#undo").on('click', function () {
    if (!hasTask()) return;

    applyUndoState(currentState-1);
  });

  $("#redo").on('click', function () {
    if (!hasTask()) return;

    applyUndoState(currentState+1);
  });

  $(document).on('keypress', function(e) {
    if (e.ctrlKey && e.keyCode == 26) {
      if (!hasTask()) return;

      applyUndoState(currentState-1);
    } else if (e.ctrlKey && e.keyCode == 25) {
      if (!hasTask()) return;

      applyUndoState(currentState+1);
    }
  });

  loadSession();
  normalizeSession();
  setupTaskSelection();
  showTaskSelection();
  $("#loading").fadeOut({duration: 500});
});

function normalizeSession() {
  // Normalize the input here
  $.each(sessions, function (i,session) {
    $.each(session.tasks, function (j,task) {
      task.assumptions = (task.assumptions || []).map(incredibleFormatTerm);
      task.conclusions = (task.conclusions || []).map(incredibleFormatTerm);
    });
  });
}

var batchSelecting = false;

function beginBatchSelect() {
  batchSelecting = true;
}

function finishBatchSelect() {
  processDerivedRule();

  batchSelecting = false;
}

graph.on('add remove change:annotation change:loading', function () {
  // Do not process the graph when loading is one, which happens during startup
  // and during batch changes.
  if (!graph.get('loading')) {
    processGraph();
  }
});
graph.on('change:selected', function () {
  // Do not process the graph when loading is one, which happens during startup
  // and during batch changes. And do not process the graph when in batch
  // selection to reduce lag.
  if (!graph.get('loading') && !batchSelecting) {
    processDerivedRule();
  }
});
graph.on('change:source change:target', function (model, end) {
  var connection_state = model.get('source').id + model.get('source').port +
    model.get('target').id + model.get('target').port;
  var connection_state_old = model.get('connection_state');
  if (connection_state != connection_state_old) {
    model.set('connection_state', connection_state);
    if (!graph.get('loading')) {
      processGraph();
    }
  }
});

paper.on('element:schieblehre:ready', saveUndo);

paper.listenTo(graph, 'batch:stop', function(evt) {
  if (evt.batchName === 'pointer') {
    saveUndo();
  }
});

// time arg is in milliseconds
// Use only to simulate heavy calculations!
// from http://stackoverflow.com/a/3048834/946226
function fakedelay(time) {
  var d1 = new Date();
  var d2 = new Date();
  while (d2.valueOf() < d1.valueOf() + time) {
    d2 = new Date();
  }
}

/* Can only be started manually */
function reset_everything() {
  $(window).off("unload"); /* Otherwise we’d just save it again */
  localStorage.removeItem("incredible-session");
  window.location.reload(false);
}

/* Can only be started manually */
function showDetails() {
    $("#graph").val(JSON.stringify(graph.toJSON(),null,2));
    $("#proof").val(JSON.stringify(buildProof(graph)));
    $("#dialog").toggle();
}
