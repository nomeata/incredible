// Some global variables
var task; // The current task
var logicName; // The name of the current logic, important for custom blocks etc.
var ruleFilter = constant_true;

// Shortcut.
var logics = examples.logics;


function constant_true (r) {
  return true;
}

function current_logic_rules() {
  return _.filter(logics[logicName].rules, ruleFilter).concat();
}
function current_custom_rules() {
  return custom_rules[logicName]||[];
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
var currentState;

function clearUndo() {
  var g = graph.toJSON();
  undoList = [{graph: g}];
  currentState = 0;
}

function saveUndo() {
  if (undefined === currentState) {
    throw Error("Usage of clearUndo() before saveUndo() is required.");
  }
  currentState += 1;
  var g = graph.toJSON();
  undoList[currentState] = {graph: g};
  undoList.length = currentState+1;
}

function applyUndoState(idx) {
  var state = undoList[idx];
  if (state) {
    graph.fromJSON(state.graph);
    currentState = idx;
    processGraph();
  }
}

function setupGraph(graph, task) {
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

  paper.scale(1.0);
  paper.setOrigin(0,0);
  rescale_paper();
}

function renderBlockDescToDraggable(blockDesc, container) {
  var el = $('<div><svg xmlns="http://www.w3.org/2000/svg"></div>');
  container.append(el);
  var vel = V(el.find("svg").get(0));

  var g = V("<g/>");
  vel.append(g);
  BlockDescRenderer(g, blockDesc, false).renderToSVG();
  gBB = g.bbox(false);
  g.translate($(el).width()/2, -gBB.y + 5);

  vel.attr({'width': container.width(), 'height': gBB.height + 10 });
  $(el).data('elementData', blockDesc.data);
  $(el).draggable({
    appendTo: "body",
    helper: "clone"
  });
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
    var blockDesc = ruleToBlockDesc(rule);
    blockDesc.isPrototype = true;
    blockDesc.canRemove = false;
    blockDesc.data = {rule: rule};
    renderBlockDescToDraggable(blockDesc, custom_container);
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
  };
}

function selectLogic(name, visible) {
  logicName = name || 'predicate';

  // Normalize the input here
  $.each(examples.logics[logicName].rules, function (_,r) {
    $.each(r.ports, function (_,p) {
      p.proposition = incredibleFormatTerm(p.proposition);
    });
  });

  if (visible) {
    ruleFilter= function (r) {
      return _.includes(visible, r.id);
    };
  } else {
    ruleFilter = constant_true;
  }

  setupPrototypeElements();
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
  setupGraph(graph, task);
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
      }
    }
  });
});

$(function (){
  $("#resettask").on('click', function () {
    setupGraph(graph, task);
  });

  $("#undo").on('click', function () {
    applyUndoState(currentState-1);
  });

  $("#redo").on('click', function () {
    applyUndoState(currentState+1);
  });


  $("#paper").on('click', saveUndo);
  $("#paper").on('wheel', saveUndo);

  loadSession();
  setupTaskSelection();
  showTaskSelection();
  $("#loading").fadeOut({duration: 500});
});


graph.on('add remove change:annotation change:loading', function () {
  // Do not process the graph when loading is one, which happens during startup
  // and during batch changes.
  if (!graph.get('loading')) {
    processGraph();
  }
});
graph.on('change:selected', function () {
  // Do not process the graph when loading is one, which happens during startup
  // and during batch changes.
  if (!graph.get('loading')) {
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
      processDerivedRule();
    }
  }
});

//graph.on('change', saveUndo);

// time arg is in milliseconds
// Use only to simulate heave calculations!
// from http://stackoverflow.com/a/3048834/946226
function fakedelay(time) {
  var d1 = new Date();
  var d2 = new Date();
  while (d2.valueOf() < d1.valueOf() + time) {
    d2 = new Date();
  }
}
