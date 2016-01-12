/*
Everything related to changing the graph display, without interaction with
other parts of the system.
*/

// A way to order the connection by creation age
var connection_counter = 0;

function create_paper() {
  return new joint.dia.Paper({
    el: $('#paper'),
    model: graph,
    width: "100%",
    height: "100%",
    gridSize: 10,
    snapLinks: true,
    defaultLink: function (elementView, magnet) {
      connection_counter++;
      return new joint.shapes.incredible.Link({
        counter : connection_counter
      });
    },

    validateConnection: function (vs, ms, vt, mt, e, vl) {
      //console.log(vs,ms,vt,mt,e,vl);

      if (ms && mt && ms.getAttribute('direction') == mt.getAttribute('direction')) {
        return false;
      }

      return true;
    }
  });
}


/**
 * Rescales the paper.
 *
 * Parameters:
 *  - amount: The ratio between the rescaled and current paper
 *            (e.g. '2.0' makes everything appear two times bigger)
 *  - x, y: Coordinates of the point that shall remain stable,
 *          given by pixel coordinates relative to the top left corner of the viewport.
 */
function paper_scale(amount, x, y) {
  var scale = V(paper.viewport).scale().sx;
  var newScale = scale*amount;
  paper.scale(newScale, newScale);

  var ox = paper.options.origin.x;
  var oy = paper.options.origin.y;
  var dx = (newScale - scale)/scale * (x - ox);
  var dy = (newScale - scale)/scale * (y - oy);
  paper.setOrigin(ox - dx, oy - dy);
}

function selectNothing() {
  $.each(graph.getElements(), function (i, el) {
    el.set('selected', false);
  });
}

$(function() {
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

  paper.on('blank:pointerdown', function (e, x, y) {
    if (e.shiftKey) { return; }

    var pos0 = {x: e.pageX, y: e.pageY};

    document.onmousemove = function(e){
      var pos1 = {x: e.pageX, y: e.pageY};
      paper.setOrigin(
        paper.options.origin.x + pos1.x - pos0.x,
        paper.options.origin.y + pos1.y - pos0.y
      );
      pos0 = pos1;
    };

    document.onmouseup = function(e){
      document.onmousemove = null;
      document.onmouseup = null;
    };
    e.stopPropagation();
  });

  paper.on('blank:pointerclick', function (evt, x, y) {
    if (evt.shiftKey) {
      // ignore
    } else {
      selectNothing();
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
        var prmpt = i18n.t('Input proposition');
        var val = cell.get('annotation');
        while (!done) {
          val = window.prompt(prmpt, val);
          if (val) {
            var prettyPrinted = incredibleFormatTerm(val);
            if (prettyPrinted) {
              done = true;
              cell.set('annotation', prettyPrinted);
            } else {
              prmpt = i18n.t('Could not parse, please try again:');
            }
          } else {
            done = true;
          }
        }
      }
    }
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
      throw Error("element:schieblehre: Unknown direction " + direction);
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
      if (dx === 0 && dy === 0) { return; }

      $.each(graph.getElements(), function (i, el) {
        if (el.get('selected') && el != model) {
          el.translate(dx,dy, { derivedMove : true });
        }
      });
      $.each(graph.getLinks(), function (i, conn) {
	var source = conn.get('source');
	var target = conn.get('target');
	var connected = source.id || target.id;
	var sourceSelected = source.id && graph.getCell(source.id).get('selected');
	var targetSelected = target.id && graph.getCell(target.id).get('selected');
	if (connected && (!source.id || sourceSelected) && (!target.id || targetSelected)) {
          conn.translate(dx,dy, { derivedMove : true });
	}
      });
    }
  });

  $("#savesvg").on('click', function (){
    save_svg("#paper svg", "incredible-proof.svg");
  });

  $("#paper").on('wheel', function (evt) {
    var up = evt.originalEvent.deltaY < 0;
    paper_scale(up ? 1.1 : 1/1.1, evt.originalEvent.offsetX, evt.originalEvent.offsetY);
  });

  $("#zoom-in").on('click', function (){
    paper_scale(1.2, $("#paper").innerWidth() / 2, $("#paper").innerHeight() / 2);
  });

  $("#zoom-out").on('click', function (){
    paper_scale(1/1.2, $("#paper").innerWidth() / 2, $("#paper").innerHeight() / 2);
   });

  $("#zoom-orig").on('click', function (){
    paper_scale(1/V(paper.viewport).scale().sx, $("#paper").innerWidth() / 2, $("#paper").innerHeight() / 2);
  });

  $("#zoom-fit").on('click', function (){
    var padding = 15;
    var cbb = paper.getContentBBox();
    if (cbb.width !== 0 && cbb.height !== 0) {
      // Only rescale and pan if we have content.
      var scale = V(paper.viewport).scale().sx;
      var w = $("#paper").innerWidth();
      var h = $("#paper").innerHeight();
      scale *= Math.min((w - padding) / cbb.width,
                       (h - padding) / cbb.height);
      paper.scale(scale, scale);

      cbb = paper.getContentBBox();
      var ox = paper.options.origin.x + (w - cbb.width) / 2 - cbb.x;
      var oy = paper.options.origin.y + (h - cbb.height) / 2 - cbb.y;
      paper.setOrigin(ox, oy);
    }
  });

});

// Separate function, to use it from the developer’s console on other SVG
// elements.
function save_svg(element, filename) {
  // Connect all CSS data that is possibly relevant
  var rules = [];
  $.each(document.styleSheets, function(sheetIndex, sheet) {
    if (sheet.ownerNode.dataset.css) {
      $.each(sheet.cssRules || sheet.rules, function(ruleIndex, rule) {
          rules.push(rule.cssText);
      });
    }
  });
  // This is wrong in general, as the SVG is not necessary the paper. How else
  // can we get the width?
  var bb = paper.getContentBBox();
  var css = rules.join("\n");
  var svg = $(element) // this way, element can be a selector, dom node or jquery object
    .clone()
    .prepend($("<style type='text/css'>").text(css))
    .attr({width: bb.x + bb.width + 10, height: bb.y + bb.height + 10})
    .wrap('<div>')
    .parent()
    .html()
    .replace(/&nbsp;/g,"&#160;");
  saveAs(new Blob([svg], {type:"application/svg+xml"}), filename);
}
