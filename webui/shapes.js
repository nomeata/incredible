
shapes = {



};

// Abstract shapes

shapes.Prototypeable = joint.shapes.basic.Generic.extend({
  initialize: function(options){
    joint.shapes.basic.Generic.prototype.initialize.apply(this, [options])

    this.on('change:position', this.hasMoved, this);
  },

  hasMoved: function(elem) {
    // When we start moving a prototype cell, remove the prototype flag
    // and create a new protoype.
    if (elem.get('prototypeElement')) {
      var newElem = elem.clone();
      newElem.set('position',newElem.get('originalPosition'));
      elem.set('prototypeElement', false);
      elem.off('change:position', this.hasMoved, this);
      // Accessing graph here -- broken?
      graph.addCell(newElem);
    }
  },
});

shapes.Gate = shapes.Prototypeable.extend({

  defaults: joint.util.deepSupplement({

    type: 'Gate',
    size: { width: 80, height: 40 },
    attrs: {
      '.': { magnet: false },
      '.body': { width: 100, height: 50 },
      circle: { r: 7, stroke: 'black', fill: 'transparent', 'stroke-width': 2 }
    }

  }, shapes.Prototypeable.prototype.defaults),
});

shapes.Gate11 = shapes.Gate.extend({

  markup: '<g class="rotatable"><g class="scalable"><image class="body"/></g><circle class="input"/><circle class="output"/></g>',

  defaults: joint.util.deepSupplement({

    type: 'Gate11',
    attrs: {
      '.input': { ref: '.body', 'ref-x': -2, 'ref-y': 0.5, magnet: 'passive', port: 'in' },
      '.output': { ref: '.body', 'ref-dx': 2, 'ref-y': 0.5, magnet: true, port: 'out' }
    }

  }, shapes.Gate.prototype.defaults)
});

shapes.Gate21 = shapes.Gate.extend({

  markup: '<g class="rotatable"><g class="scalable"><image class="body"/><text/></g><circle class="input input1"/><circle  class="input input2"/><circle class="output"/></g>',

  defaults: joint.util.deepSupplement({

    type: 'logic.Gate21',
    attrs: {
      '.input1': { ref: '.body', 'ref-x': -2, 'ref-y': 0.3, magnet: 'passive', port: 'in1' },
      '.input2': { ref: '.body', 'ref-x': -2, 'ref-y': 0.7, magnet: 'passive', port: 'in2' },
      '.output': { ref: '.body', 'ref-dx': 2, 'ref-y': 0.5, magnet: true, port: 'out' }
    }

  }, shapes.Gate.prototype.defaults)

});

// Concrete shape. Should be named like the rule in the logic (for now)

shapes.conjI = shapes.Gate21.extend({

  defaults: joint.util.deepSupplement({

    rule: 'conjI',
    attrs: {
      image: { 'xlink:href': 'images/conjI.svg' },
      text: { text: 'conjI', 'ref-x': 27, 'ref-y': 0.5, 'font-size': 10 }
    }

  }, shapes.Gate21.prototype.defaults),

  operation: function(input) {
    return input;
  }

});

shapes.genericBlock = shapes.Prototypeable.extend(_.extend({}, joint.shapes.basic.PortsModelInterface, {

    markup: '<g class="rotatable"><g class="scalable"><rect class="body"/></g><text class="label"/><g class="inPorts"/><g class="outPorts"/></g>',
    portMarkup: '<g class="port port<%= id %>"><circle class="port-body"/><text class="port-label"/></g>',

    defaults: joint.util.deepSupplement({

        type: 'devs.Model',
        size: { width: 1, height: 1 },

        inPorts: [],
        outPorts: [],

        attrs: {
            '.': { magnet: false },
            '.body': {
                width: 150, height: 250,
                stroke: '#000000'
            },
            '.port-body': {
                r: 10,
                magnet: true,
                stroke: '#000000'
            },
            text: {
                'pointer-events': 'none'
            },
            '.label': { text: 'Model', 'ref-x': .5, 'ref-y': 10, ref: '.body', 'text-anchor': 'middle', fill: '#000000' },
            '.port-label': { 'font-size': '8px' },
            '.inPorts .port-label': { x:-15, dy: 4, 'text-anchor': 'end', fill: '#000000' },
            '.outPorts .port-label':{ x: 15, dy: 4, fill: '#000000' }
        }

    }, shapes.Prototypeable.prototype.defaults),

    getPortAttrs: function(portSpec, index, total, selector, type) {

        var attrs = {};

        var portClass = 'port' + index;
        var portSelector = selector + '>.' + portClass;
        var portLabelSelector = portSelector + '>.port-label';
        var portBodySelector = portSelector + '>.port-body';

        attrs[portLabelSelector] = { text: portSpec.text };
        attrs[portBodySelector] = { port: { id: portSpec.id || _.uniqueId(type) , type: type } };
        attrs[portSelector] = { ref: '.body', 'ref-y': (index + 0.5) * (1 / total) };

        if (selector === '.outPorts') { attrs[portSelector]['ref-dx'] = 0; }

        return attrs;
    }
}));

shapes.impI = shapes.genericBlock.extend({
    defaults: joint.util.deepSupplement({
            rule: 'impI',
            inPorts: [
                {text: 'B', id: 'in'},
            ],
            outPorts: [
                {text: 'A', id: 'hyp'},
                {text: 'imp(A,B)', id: 'out'},
            ],
            size: { width: 80, height: 50 },
            attrs: { '.label': {text: 'impI'}},
    }, shapes.genericBlock.prototype.defaults),
});

shapes.impE = shapes.genericBlock.extend({
    defaults: joint.util.deepSupplement({
            rule: 'impE',
            inPorts: [
                {text: 'imp(A,B)', id: 'in1'},
                {text: 'A', id: 'in2'},
            ],
            outPorts: [
                {text: 'B', id: 'out'},
            ],
            size: { width: 80, height: 50 },
            attrs: { '.label': {text: 'impE'}},
    }, shapes.genericBlock.prototype.defaults),
});

shapes.conjE = shapes.genericBlock.extend({
    defaults: joint.util.deepSupplement({
            rule: 'conjE',
            inPorts: [
                {text: 'and(A,B)', id: 'in'},
            ],
            outPorts: [
                {text: 'A', id: 'out'},
                {text: 'B', id: 'out'},
            ],
            size: { width: 80, height: 50 },
            attrs: { '.label': {text: 'conjE'}},
    }, shapes.genericBlock.prototype.defaults),
});


// Assumptions and conclusions
shapes.IO = shapes.Gate.extend({

    markup: '<g class="rotatable"><g class="scalable"><rect class="body"/></g><path class="wire"/><circle/><text/></g>',

    defaults: joint.util.deepSupplement({

        type: 'logic.IO',
        size: { width: 60, height: 30 },
        attrs: {
            '.body': { fill: 'white', stroke: 'black', 'stroke-width': 2 },
            '.wire': { ref: '.body', 'ref-y': .5, stroke: 'black' },
            text: {
                fill: 'black',
                ref: '.body', 'ref-x': .5, 'ref-y': .5, 'y-alignment': 'middle',
                'text-anchor': 'middle',
		'font-family': 'sans-serif',
                'font-size': '12px'
            }
        }

    }, shapes.Gate.prototype.defaults),

    initialize: function(options){
	shapes.Gate.prototype.initialize.apply(this, [options])
	// TODO: resize accoring to the text
    },
});

shapes.Assumption = shapes.IO.extend({

    defaults: joint.util.deepSupplement({

        type: 'logic.Input',
        attrs: {
            '.wire': { 'ref-dx': 0, d: 'M 0 0 L 23 0' },
            circle: { ref: '.body', 'ref-dx': 30, 'ref-y': 0.5, magnet: true, 'class': 'output', port: 'out' },
        }

    }, shapes.IO.prototype.defaults)
});

shapes.Conclusion = shapes.IO.extend({

    defaults: joint.util.deepSupplement({

        type: 'logic.Output',
        attrs: {
            '.wire': { 'ref-x': 0, d: 'M 0 0 L -23 0' },
            circle: { ref: '.body', 'ref-x': -30, 'ref-y': 0.5, magnet: 'passive', 'class': 'input', port: 'in' },
        }

    }, shapes.IO.prototype.defaults)

});

