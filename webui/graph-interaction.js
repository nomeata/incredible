/*
Everything related to changing the graph display, without interaction with
other parts of the system.
*/

function create_paper() {
  return new joint.dia.Paper({
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
}


function rescale_paper() {
  var paper_w = $("#paper").innerWidth() - 5;
  var paper_h = $("#paper").innerHeight() - 5;

  var bb = paper.getContentBBox();

  var w = Math.max(paper_w, bb.x + bb.width);
  var h = Math.max(paper_h, bb.y + bb.height);
  paper.setDimensions(w, h);
}

$(window).on('resize load', rescale_paper);

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

});
