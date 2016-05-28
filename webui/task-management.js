var task_desc; // A string describing the current task

var tasks_saved = {};
var tasks_solved = {};
var custom_tasks = {};
var custom_rules = {};
var session_passwords = [];

var funnyUnicodeCharacters="⚛☃⚾♛♬☏⚒☸☀☮☘☭";

var hasStorage = (function() {
  try {
    localStorage.setItem('test', 'text');
    localStorage.removeItem('text');
    return true;
  } catch (exception) {
    return false;
  }
}());

function saveTask() {
  if (task_desc) {
    tasks_saved[task_desc] = _.omit(graph.toJSON(), 'loading');
    tasks_solved[task_desc] = graph.get('qed');
    saveSession();
  }
}

function saveSession() {
  if (hasStorage) {
    localStorage["incredible-session"] = JSON.stringify({
      saved: tasks_saved,
      solved: tasks_solved,
      custom: custom_tasks,
      rules: custom_rules,
      passwords: session_passwords
    });
  }
}

function loadSession() {
  if (hasStorage && localStorage["incredible-session"]) {
    var stored = JSON.parse(localStorage["incredible-session"]);
    tasks_saved = stored.saved || {};
    tasks_solved = stored.solved || {};
    custom_tasks = stored.custom || {};
    custom_rules = stored.rules || {};
    session_passwords = stored.passwords || [];
  }
}
function taskToDesc(logic, task) {
  return JSON.stringify([logic, task.assumptions, task.conclusions]);
}

function selectSessionTask(evt) {
  saveTask();

  var target = $(evt.currentTarget);
  var session = sessions[target.data('session')];
  var thisTask;
  if (target.data('task') !== undefined) {
    thisTask = session.tasks[target.data('task')];
  } else if (target.data('custom_task') !== undefined) {
    thisTask = custom_tasks[session.name][target.data('custom_task')];
  } else {
    throw Error("selectSessionTask");
  }

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

function taskToHTML(task, onRemove) {
  var container = $("<div class='inferencerule'>");
  d1 = $("<ul class='assumptions'>");
  $.each(task.assumptions || [], function (i, el) {
    d1.append($("<li>").text(incredibleFormatTerm(el)));
  });
  d2 = $("<ul class='conclusions'>");
  $.each(task.conclusions || [], function (i, el) {
    d2.append($("<li>").text(incredibleFormatTerm(el)));
  });
  if (!!onRemove) {
    var tool = $('<div class="task-tools"><svg xmlns="http://www.w3.org/2000/svg" width="32px" height="32px"></div>');
    V(tool.find("svg").get(0))
      .append(render_delete_tool("Remove task.").translate(16,16));
    tool
      .on('click', onRemove)
      .appendTo(container);
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
  $("#sessiontasks").empty();
  $.each(sessions, function (i,session) {
    if (!session.password || $.inArray(session.password, session_passwords) >= 0) {
      $("<h3>").text(i18n.t(session.name)).appendTo("#sessiontasks");
      var container = $("<div>").addClass("tasklist").appendTo("#sessiontasks");
      $.each(session.tasks, function (j,thisTask) {
        taskToHTML(thisTask)
          .addClass("sessiontask")
          .data({session: i, task: j, desc: taskToDesc(session.logic||'predicate', thisTask)})
          .on('click', with_graph_loading(selectSessionTask))
          .appendTo(container);
      });

      $.each(custom_tasks[session.name] || [], function (j,thisTask) {
        taskToHTML(thisTask,
            function () {
              custom_tasks[session.name].splice(j, 1);
              setupTaskSelection();
            }
         ).addClass("sessiontask")
          .data({session: i, custom_task: j, desc: taskToDesc(session.logic||'predicate', thisTask)})
          .on('click', with_graph_loading(selectSessionTask))
          .appendTo(container);
      });

      var textarea = $('<textarea>A&#10;────────────&#10;A</textarea>');

      $('<div id="customtasks" class="tasklist">')
        .append($('<div class="customtaskentry">')
          .append(textarea)
          .append($('<button>')
            .text(i18n.t('Add'))
            .attr('title', i18n.t('add-title'))
            .on('click', function (){
              var thisTask = taskFromText(textarea.val());
              if (thisTask) {
                if (!custom_tasks[session.name]) {
                  custom_tasks[session.name] = [];
                }
                custom_tasks[session.name].push(thisTask);
                setupTaskSelection();
                saveSession();
              } else {
                alert(i18n.t('task-parse-error'));
              }
            })
          )
        )
        .appendTo(container);
    } else {
      $("<h3>").text(i18n.t(session.name)).appendTo("#sessiontasks");
      var button;
      var input;
      input = $("<input type='text'>")
        .attr('placeholder',i18n.t('password'))
        .on('keydown',function(e) {
          if (e.keyCode == 13) {
            button.click();
          }
        })
        .appendTo("#sessiontasks");
      button = $("<button>")
        .text(i18n.t('Unlock'))
        .on('click',function() {
          if (input.val() == session.password) {
            session_passwords.push(session.password);
            setupTaskSelection();
          } else {
            alert(i18n.t('wrong-password'));
          }
          return true;
        })
        .appendTo("#sessiontasks");
    }
  });

  updateTaskSelectionInfo();
}

$(function (){
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
      rule.containedBlocks = countBlocks(true);
      custom_rules[logicName].push(rule);
      setupPrototypeElements();
      selectNothing();
    }
  });
});

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
        task[now].push(prop);
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

