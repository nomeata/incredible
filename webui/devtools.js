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

function selectNoTask() {
  $("#taskselect").val("");
  $("#task").hide();
  selectLogic('predicate');
  saveTask();
  task = { };
  task_desc = undefined;
  setupGraph(graph, task);
  processGraph();
}

function selectNamedTask(name) {
  saveTask();
  selectLogic(examples.tasks[name].logic);
  loadTask(examples.tasks[name]);
  task_desc = undefined;
  $("#taskselect").val(name);
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
    if (el.get('prototypeElement')) {el.remove();}
  });
  graph.set('loading', false);
}


$(function (){
  $("#taskselect").change(with_graph_loading(function () {
    if (this.value) {
      selectNamedTask(this.value);
    }
  }));
  $("#proofselect").change(with_graph_loading (function () {
    if (this.value) {
      selectProof(this.value);
    }
  }));
  $("#freeproof").click(with_graph_loading (function () { selectNoTask(); }));

  $("#showdialog").click(function(){
    $("#graph").val(JSON.stringify(graph.toJSON(),null,2));
    $("#proof").val(JSON.stringify(buildProof(graph)));
    $("#dialog").toggle();
  });
  $("#closedialog").click(function(){
    $("#dialog").hide();
  });

  $("#showhelp").click(function(){
    $("#help").toggle();
  });
  $("#closehelp").click(function(){
    $("#help").hide();
  });

  $("#reset").click(function(){
    if (window.confirm(i18n.t("confirm-reset"))) {
      $(window).off("unload"); /* Otherwise we’d just save it again */
      localStorage.removeItem("incredible-session");
      window.location.reload(false);
    }
  });

  $("#inferredrule #inferredrulewrapper").draggable({
    appendTo: "body",
    helper: "clone"
    // helper: function ()  { return $("<span>Hi</span>") }
  });
});
