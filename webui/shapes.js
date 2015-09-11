/*
 * Specification for Element representing blocks in the proof:
 *
 * Regular proof blocks:
 *   - The cell has an attribute 'rule', which is the rule object of the logic.
 *   - Every SVG element that is an magnet (magnet: true) has an attribute 'port',
 *     that corresponds to the name of the port in the logic, e.g. "in2" or "hyp".
 * Assumption blocks:
 *   - The cell has an attribute 'assumption', with the number of the assumption it
 *     represents.
 * Conclusion blocks:
 *   - The cell has an attribute 'conclusion', with the number of the assumption it
 *     represents.
 *
 * Elements with an attribute 'prototypeElement' set to true are not part of the
 * proof, but rather the elements one can drag into the paper.
 *
 * Furthermore, every magnet has an attribute 'direction', which is either "in" or
 * "out". This is used by the UI to make sure wires are connected properly.
 *
 */


shapes = {};


/*
 * How does jointJS find the view for a model?
 * It takes the 'type' field, of the model, excepts it to be foo.bar
 * and then looks at join.shapes[foo][bar+'View']....
 */


joint.shapes.incredible = {}


/*
 * The new way of doing things: No more visual information in the model,
 * the shape is calculated in the view.
 * The whole crazy `attrs` feature of JointJS is not used!
 */
joint.shapes.incredible.Generic = joint.shapes.basic.Generic.extend({
  defaults: joint.util.deepSupplement({
    type: 'incredible.Generic',
  }, joint.shapes.basic.Generic.prototype.defaults)
});

joint.shapes.incredible.GenericView = joint.dia.ElementView.extend({
  initialize: function () {
    joint.dia.ElementView.prototype.initialize.apply(this, arguments);

    // Listen to some incredible-specific change events
    this.listenTo(this.model, 'change:brokenPorts', this.update);
  },

  renderMarkup: function() {
    var rule = this.model.get('rule');
    var isPrototype = this.model.get('prototypeElement');

    this.vel.attr('magnet',false);

    var group = V("<g/>");
    this.vel.append(group);

    var rect = V("<rect fill='#ecf0f1' rx='5' ry='5' width='80' height='30' stroke='#bdc3c7' stroke-opacity='0.5'/>");
    group.append(rect);

    rectBB = rect.bbox(true);

    var text = V("<text class='label' font-family='sans' fill='black'/>");
    text.text(rule.id);
    group.append(text);
    textBB = text.bbox(true);
    // center text
    text.translate((rectBB.width - textBB.width)/2, (rectBB.height - textBB.height)/2);

    var ports = rule.ports;
    var portsList = _.sortBy(_.map(ports, function (v, i) {return _.extend({id: i}, v);}), 'id');
    var portsGroup = _.groupBy(portsList, "type");

    _.each(portsGroup, function (thesePorts, portType) {
      var total = _.size(thesePorts);
      _.each(thesePorts, function (portDesc, index) {
        var direction = ({assumption: 'in', conclusion: 'out', 'local hypothesis': 'out'})[portType];
          var pacman = V('<path class="port-body" stroke="none" fill="#777" magnet="true"/>');
          pacman.attr({port: portDesc.id, direction: direction});

          group.append(pacman);

          if (isPrototype) {
            var label = V("<text font-family='sans' font-size='8px'/>");
            label.text(portDesc.proposition);
            group.append(label);
            var labelBB = label.bbox(true);
          }

          var labelPad = 7;

          if (portType === 'assumption') {
            // put left
            var y = (index+0.5)/total * rectBB.height;
            pacman.translate( 0, y );
            pacman.rotate(135);
            pacman.attr({d: "M0,0 l 0 5 a5,5 0 1,1 5,-5 z"});
            if (isPrototype) {
              label.translate( -labelBB.width - labelPad, y - labelBB.height/2 );
            }
          } else if (portType === 'conclusion') {
            // put right
            var y = (index+0.5)/total * rectBB.height;
            pacman.translate( rectBB.width, (index+0.5)/total * rectBB.height );
            pacman.rotate(135);
            pacman.attr({d: "M-5,-5 l 0 5 a5,5 0 1,0 5,-5 z"});
            if (isPrototype) {
              label.translate( rectBB.width + labelPad, y - labelBB.height/2 );
            }
          } else if (portType === 'local hypothesis') {
            // put below
            var x = (index+0.5)/total * rectBB.width;
            pacman.translate( x, rectBB.height);
            pacman.rotate(225);
            pacman.attr({d: "M-5,-5 l 0 5 a5,5 0 1,0 5,-5 z"});
            if (isPrototype) {
              label.translate( x - labelBB.width/2, rectBB.height + labelPad);
            }
          } else {
            throw new Error("initialize(): Unknown portType");
          }
      });
    });
  },
  update: function () {
    // Do our on stuff
    var rule = this.model.get('rule');
    var brokenPorts = this.model.get('brokenPorts') || {};

    _.each(this.vel.find(".port-body"), function (port) {
      if (V(port).attr('port') in brokenPorts) {
        V(port).attr('fill', '#F00');
      } else {
        V(port).attr('fill', '#777');
      }
    });

    // Just in case we use the attrs feature, let's call the jointjs update function
    joint.dia.ElementView.prototype.update.apply(this, arguments);
  }
});

// Assumptions and conclusions
joint.shapes.incredible.ACBlock = joint.shapes.basic.Generic.extend({

  type: 'incredible.ACBlock',

  markup: '<g class="rotatable"><g class="scalable"><rect class="body"/></g><path class="wire"/><circle/><text/></g>',

  defaults: joint.util.deepSupplement({

    type: 'logic.IO',
    size: {width: 60, height: 30},
    attrs: {
      '.': {magnet: false},
      '.body': {fill: 'white', stroke: 'black', 'stroke-width': 2, width: 100, height: 50},
      '.wire': {ref: '.body', 'ref-y': .5, stroke: 'black'},
      circle: {r: 7, stroke: 'black', fill: 'transparent', 'stroke-width': 2, magnet: true},
      text: {
        fill: 'black',
        ref: '.body', 'ref-x': .5, 'ref-y': .5, 'y-alignment': 'middle',
        'text-anchor': 'middle',
        'font-family': 'sans-serif',
        'font-size': '12px'
      }
    }
  }),

  initialize: function (options) {
    this.constructor.__super__.constructor.__super__.initialize.apply(this, arguments);
    // TODO: resize accoring to the text
  },
});

joint.shapes.incredible.ACBlockView = joint.dia.ElementView.extend({
  update: function () {
    /* Doesn’t work
     w = this.$('text')[0].getBBox().width;
     if (w > 0) {
     console.log(w);
     V(this.$('.body').get(0)).attr('width', w);
     }
     */
    joint.dia.ElementView.prototype.update.apply(this, arguments);
  },

});
joint.shapes.incredible.AssumptionView = joint.shapes.incredible.ACBlockView
joint.shapes.incredible.ConclusionView = joint.shapes.incredible.ACBlockView

joint.shapes.incredible.Assumption = joint.shapes.incredible.ACBlock.extend({

  defaults: joint.util.deepSupplement({

    type: 'incredible.Assumption',
    attrs: {
      '.wire': {'ref-dx': 0, d: 'M 0 0 L 23 0'},
      circle: {ref: '.body', 'ref-dx': 30, 'ref-y': 0.5, direction: 'out'},
    }

  }, joint.shapes.incredible.ACBlock.prototype.defaults),
});

joint.shapes.incredible.Conclusion = joint.shapes.incredible.ACBlock.extend({

  defaults: joint.util.deepSupplement({

    type: 'incredible.Conclusion',
    attrs: {
      '.wire': {'ref-x': 0, d: 'M 0 0 L -23 0'},
      circle: {ref: '.body', 'ref-x': -30, 'ref-y': 0.5, direction: 'in'},
    }

  }, joint.shapes.incredible.ACBlock.prototype.defaults)

});

