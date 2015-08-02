
shapes = {



};

shapes.Gate = joint.shapes.basic.Generic.extend({

  defaults: joint.util.deepSupplement({

    type: 'Gate',
    size: { width: 80, height: 40 },
    attrs: {
      '.': { magnet: false },
      '.body': { width: 100, height: 50 },
      circle: { r: 7, stroke: 'black', fill: 'transparent', 'stroke-width': 2 }
    }

  }, joint.shapes.basic.Generic.prototype.defaults),

  operation: function() { return true; }
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

shapes.ConjI = shapes.Gate21.extend({

  defaults: joint.util.deepSupplement({

    type: 'conjI',
    attrs: {
      image: { 'xlink:href': 'images/conjI.svg' },
      text: { text: 'conjI', 'ref-x': 27, 'ref-y': 0.5, 'font-size': 10 }
    }

  }, shapes.Gate21.prototype.defaults),

  operation: function(input) {
    return input;
  }

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

