var graph = new joint.dia.Graph;

var paper = new joint.dia.Paper({

  el: $('#paper'), model: graph,
  width: 1000, height: 600, gridSize: 5,
  snapLinks: true,
  defaultLink: new joint.shapes.logic.Wire,

  validateConnection: function (vs, ms, vt, mt, e, vl) {

    if (e === 'target') {

      // target requires an input port to connect
      if (!mt || !mt.getAttribute('class') || mt.getAttribute('class').indexOf('input') < 0) return false;

      // check whether the port is being already used
      var portUsed = _.find(this.model.getLinks(), function (link) {

        return (link.id !== vl.model.id &&
        link.get('target').id === vt.model.id &&
        link.get('target').port === mt.getAttribute('port'));
      });

      return !portUsed;

    } else { // e === 'source'

      // source requires an output port to connect
      return ms && ms.getAttribute('class') && ms.getAttribute('class').indexOf('output') >= 0;
    }
  }
});

// zoom the viewport by 50%
paper.scale(1.5, 1.5);

// Diagram setup
var task = examples.tasks.conjself;
var logic = examples.logics[task.logic];


var gates = {};

// Fixed blocks for input and output
$.each(task.conclusions, function (i,c) {
  var n = i+1;
  var gate = new shapes.Conclusion({
	position: {x: 450, y: 100 + 50 * i},
	attrs: { text: {text: c}},
	conclusion: n,
	});
  graph.addCell(gate);
});
$.each(task.assumptions, function (i,c) {
  var n = i+1;
  var gate = new shapes.Assumption({
	position: {x: 50, y: 100 + 50 * i},
	attrs: { text: {text: c}},
	assumption: n,
	});
  graph.addCell(gate);
});

// "Prototype blocks" for each element
$.each(logic.rules, function(i,rule) {
  var n = i+1;
  var elem = new shapes[rule.id]({
	position: {x: 550, y: 100 + 50 * i},
	prototypeElement: true,
	});
  graph.addCell(elem);
});


function buildProof(graph) {
    var proof = {}

    proof.blocks = {}
    graph.getElements().map(
        function (e,i) {
			c = graph.getCell(e.id);
			if (c.get('assumption') || c.get('conclusion')) {
				return;
			}
            proof.blocks[e.id] = {};
            proof.blocks[e.id]['rule'] = e.attributes.type;
        });

    proof.connections = {}
    graph.getLinks().map(
        function(l,i) {
            con = {}
            con.from = makeConnEnd(graph, l.get('source'));
            con.to = makeConnEnd(graph, l.get('target'));
            proof.connections[l.id] = con;
        });
    return proof;
}

function makeConnEnd(graph, x) {
    ret = {};
	c = graph.getCell(x.id);
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
