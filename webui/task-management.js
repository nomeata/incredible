var task_desc; // A string describing the current task
sessions.custom = {tasks: []};

var tasks_saved = {};
var tasks_solved = {};

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
    custom: sessions.custom
  });
}

function loadSession() {
  if (localStorage["incredible-session"]) {
    var stored = JSON.parse(localStorage["incredible-session"]);
    tasks_saved = stored.saved || {};
    tasks_solved = stored.solved || {};
    sessions.custom = stored.custom || {tasks: []};
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
    taskToHTML(thisTask)
      .addClass("sessiontask")
      .data({session: 'custom', task: j, desc: taskToDesc(sessions.custom.logic||'predicate', thisTask)})
      .on('click', with_graph_loading(selectSessionTask))
      .insertBefore("#customtask");
  });

  $("#customtask #addcustomtask").on('click', function (){
    var thisTask = taskFromText($("#customtask textarea").val());
    if (thisTask) {
      var j = sessions.custom.tasks.length;
      sessions.custom.tasks.push(thisTask);
      taskToHTML(thisTask)
        .addClass("sessiontask")
        .data({session: 'custom', task: j, desc: taskToDesc(sessions.custom.logic||'predicate', thisTask)})
        .on('click', with_graph_loading(selectSessionTask))
        .insertBefore("#customtask");
      updateTaskSelectionInfo(); // A bit overhead re-doing all of them, but that’s ok, I hope
      saveSession();
    } else {
      alert(i18n.t('task-parse-error'));
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

