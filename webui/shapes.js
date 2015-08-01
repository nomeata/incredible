
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
