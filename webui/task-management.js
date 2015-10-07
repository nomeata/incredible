var task_desc; // A string describing the current task
sessions.custom = {tasks: []};

var tasks_saved = {};
var tasks_solved = {};
var custom_rules = {};

var funnyUnicodeCharacters="⚛☃⚾♛♬☏⚒☸☀☮☘☭";

function saveTask() {
  if (task_desc) {
    tasks_saved[task_desc] = _.omit(graph.toJSON(), 'loading');
    tasks_solved[task_desc] = graph.get('qed');
    saveSession();
  }
}

function saveSession() {
  localStorage["incredible-session"] = JSON.stringify({
    saved: tasks_saved,
    solved: tasks_solved,
    custom: sessions.custom,
    rules: custom_rules
  });
}

function loadSession() {
  if (localStorage["incredible-session"]) {
    var stored = JSON.parse(localStorage["incredible-session"]);
    tasks_saved = stored.saved || {};
    tasks_solved = stored.solved || {};
    sessions.custom = stored.custom || {tasks: []};
    custom_rules = stored.rules || {};
  }
}
function taskToDesc(logic, task) {
  return JSON.stringify([logic, task.assumptions, task.conclusions]);
}

function selectSessionTask(evt) {
  saveTask();

  var session = sessions[$(evt.currentTarget).data('session')];
  var thisTask = session.tasks[$(evt.currentTarget).data('task')];

  task_desc = $(evt.currentTarget).data('desc');

  selectLogic(session.logic, session["visible-rules"]);

  setupPrototypeElements();

  loadTask(thisTask);
  if (tasks_saved[task_desc]) {
    graph.fromJSON(tasks_saved[task_desc]);
  }

  // Start the animation at the end
  $("#taskdialog").hide("slide", {direction:"down"}, 800, function () {
    $("#taskbottombar").show("slide", {direction:"down"}, 100);
  });
}

function taskToHTML(task, canRemove, idx) {
  var container = $("<div class='inferencerule'>");
  d1 = $("<ul class='assumptions'>");
  $.each(task.assumptions || [], function (i, el) {
    d1.append($("<li>").text(incredibleFormatTerm(el)));
  });
  d2 = $("<ul class='conclusions'>");
  $.each(task.conclusions || [], function (i, el) {
    d2.append($("<li>").text(incredibleFormatTerm(el)));
  });
  if (!!canRemove) {
    var tool = $('<div class="tool-remove"><svg xmlns="http://www.w3.org/2000/svg" width="32px" height="32px" transfomr="translate(50,-17.5)"></div>');
    var svg = V(tool.find("svg").get(0));
    var markup = ['<g transform="translate(16,16)">',
        '<circle r="11" />',
        '<path transform="scale(.8) translate(-16, -16)" d="M24.778,21.419 19.276,15.917 24.777,10.415 21.949,7.585 16.447,13.087 10.945,7.585 8.117,10.415 13.618,15.917 8.116,21.419 10.946,24.248 16.447,18.746 21.948,24.248z"/>',
        '<title>Remove task.</title>',
        '</g>'].join('');
    svg.append(V(markup));
    tool.on('click', function () {
      sessions.custom.tasks.splice(idx, 1);
      container.css('display', 'none');
    });

    container.append(tool);
  }
  return container.append(d1, $("<hr/>"), d2); 
}

$(function () {
  $("#taskbottombar").on('click', function () {
    $("#taskbottombar").stop();
    saveTask(); // to update session_saved
    showTaskSelection();
  });
});

function setupTaskSelection() {
  $.each(sessions, function (i,session) {
    $("<h3>").text(i18n.t(session.name)).appendTo("#sessiontasks");
    var container = $("<div>").addClass("tasklist").appendTo("#sessiontasks");
    $.each(session.tasks, function (j,thisTask) {
      taskToHTML(thisTask)
        .addClass("sessiontask")
        .data({session: i, task: j, desc: taskToDesc(session.logic||'predicate', thisTask)})
        .on('click', with_graph_loading(selectSessionTask))
        .appendTo(container);
    });
  });

  $.each(sessions.custom.tasks, function (j,thisTask) {
    taskToHTML(thisTask, true, j)
      .addClass("sessiontask")
      .data({session: 'custom', task: j, desc: taskToDesc(sessions.custom.logic||'predicate', thisTask)})
      .on('click', function (evt) {
        if (evt.target == this) {
          with_graph_loading(selectSessionTask)(evt);
        }
      })
      .insertBefore("#customtask");
  });

  updateTaskSelectionInfo();

  $("#customtask #addcustomtask").on('click', function (){
    var thisTask = taskFromText($("#customtask textarea").val());
    if (thisTask) {
      var j = sessions.custom.tasks.length;
      sessions.custom.tasks.push(thisTask);
      taskToHTML(thisTask, true, sessions.custom.tasks.length - 1)
        .addClass("sessiontask")
        .data({session: 'custom', task: j, desc: taskToDesc(sessions.custom.logic||'predicate', thisTask)})
        .on('click', function (evt) {
          if (evt.target == this) {
            with_graph_loading(selectSessionTask)(evt);
          }
        })
        .insertBefore("#customtask");
      updateTaskSelectionInfo(); // A bit overhead re-doing all of them, but that’s ok, I hope
      saveSession();
    } else {
      alert(i18n.t('task-parse-error'));
    }
  });

  $("#addcustomblock").on('click', function (){
    // Instead of reading the displayed rule, we simply re-calculate it. It
    // should be the same, if not, then that is a bug..
    var proof = buildProof(graph, derivedRuleBlockSelector);
    var rule = incredibleNewRule(current_logic(), proof);
    if ($.isEmptyObject(proof.blocks) ||
        typeof rule === 'string' ||
        rule instanceof String ||
        rule === null ||
        rule === undefined) {
      alert('Could not produce a usable rule');
    } else {
      if (!custom_rules[logicName]) {
        custom_rules[logicName] = [];
      }
      var n = custom_rules[logicName].length + 1;
      rule.id = "custom" + n;
      rule.desc = {label: next_custom_block_name()};
      custom_rules[logicName].push(rule);
      setupPrototypeElements();
      selectNothing();
    }
  });
}

function next_custom_block_name(){
  var n = (custom_rules[logicName] || []).length + 1;
  var m = funnyUnicodeCharacters.length;
  var res = "";
  while (n > 0) {
    res = res + funnyUnicodeCharacters.substr(n % m,1);
    n = Math.floor(n/m);
  }
  return res;
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
    return;
  }
}

function updateTaskSelectionInfo() {
  var lookupmap = {};
  $(".sessiontask").each(function (i,t) {
    var desc = $(t).data('desc');
    lookupmap[desc] = lookupmap[desc]||[];
    lookupmap[desc].push(t);
  });
  $.each(tasks_saved, function (desc, proof) {
    $.each(lookupmap[desc]||[], function (i,t) {
      $(t).addClass('attempted');
    });
  });
  $.each(tasks_solved, function (desc, qed) {
    $.each(lookupmap[desc]||[], function (i,t) {
      $(t).toggleClass('solved', qed);
    });
  });
}

function showTaskSelection() {
  updateTaskSelectionInfo();
  $("#taskbottombar").hide("slide", {direction: "down"}, 100, function () {
    $("#taskdialog").show("slide", {direction:"down"}, 800, function () {
      $("#taskdialog").focus();
    });
  });
}

$(function (){
  $(window).on('unload', function () {
    saveTask();
    saveSession();
  });
});

