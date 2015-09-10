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
 * The followsing is derived from Ports{Model,View}Interface in
 * joint.shapes.basic, for customization
 */

joint.shapes.incredible.Generic = joint.shapes.basic.Generic.extend({

  markup: '<g class="rotatable"><g class="scalable"><rect class="body"/></g><text class="label"/><g class="ports"/></g>',
  portMarkup: '<g class="port port<%= id %>">' +
//     '<circle class="port-body"/>'  +
    // pacman open towards left
  '<path class="port-body" stroke="none" fill="#777" transform="rotate(135)" d="M0,0 l 0 5 a5,5 0 1,1 5,-5 z"/>' +
  '<text class="port-label"/>' +
  '</g>',

  defaults: joint.util.deepSupplement({

    type: 'incredible.Generic',
    size: {width: 80, height: 40},

    attrs: {
      '.': {magnet: false},
      '.body': {
        width: 150, height: 250,
        stroke: '#000000'
      },
      '.port-body': {
        r: 10,
        magnet: true,
      },
      text: {
        'pointer-events': 'none'
      },
      '.label': {text: 'Model', 'ref-x': .5, 'ref-y': 10, ref: '.body', 'text-anchor': 'middle', fill: '#000000'},
      '.port-label': {'font-size': '8px'},
    }

  }, joint.shapes.basic.Generic.prototype.defaults),


  initialize: function () {
    this.updatePortAttributes();
    this.on('change:rule change:brokenPorts change:prototypeElement', this.updatePortAttributes, this);
    this.constructor.__super__.constructor.__super__.initialize.apply(this, arguments);
  },

  updatePortAttributes: function () {
    var attrs = {};

    var ruleObj = this.get('rule');
    var isPrototype = this.get('prototypeElement');
    var brokenPorts = this.get('brokenPorts') || {};

    attrs['.label'] = {text: ruleObj.id};

    var rules = ruleObj.ports;
    var rulesList = _.sortBy(_.map(rules, function (v, i) {return _.extend({id: i}, v);}), 'id');
    var rulesGroup = _.groupBy(rulesList, "type");

    _.each(rulesGroup, function (thesePorts, portType) {
      var total = _.size(thesePorts);
      _.each(thesePorts, function (portDesc, index) {
        var direction = ({assumption: 'in', conclusion: 'out', 'local hypothesis': 'out'})[portType];
        var selector = '.ports';

        var portClass = 'port' + portDesc.id;
        var portSelector = selector + '>.' + portClass;
        var portLabelSelector = portSelector + '>.port-label';
        var portBodySelector = portSelector + '>.port-body';

        attrs[portBodySelector] = {port: portDesc.id, direction: direction};
        attrs[portSelector] = {ref: '.body'};

        if (portType === 'assumption') {
          attrs[portSelector]['ref-y'] = (index + 0.5) * (1 / total);
          attrs[portBodySelector].transform = 'rotate(135)';
          attrs[portLabelSelector] = {dx: -10, y: 5, 'y-alignment': 'middle', 'text-anchor': 'end', fill: '#000000'};
        } else if (portType === 'conclusion') {
          attrs[portSelector]['ref-y'] = (index + 0.5) * (1 / total);
          attrs[portSelector]['ref-dx'] = 0;
          attrs[portBodySelector].transform = 'rotate(135)';
          attrs[portBodySelector].d = 'M-5,-5 l 0 5 a5,5 0 1,0 5,-5 z';
          attrs[portLabelSelector] = {dx: 10, y: 5, 'y-alignment': 'middle', fill: '#000000'};
        } else if (portType === 'local hypothesis') {
          attrs[portSelector]['ref-x'] = (index + 0.5) * (1 / total);
          attrs[portSelector]['ref-dy'] = 1;
          attrs[portBodySelector].transform = 'rotate(-135)';
          attrs[portBodySelector].d = 'M-5,-5 l 0 5 a5,5 0 1,0 5,-5 z';
          attrs[portLabelSelector] = {x: 0, y: 20, 'text-anchor': 'middle', fill: '#000000'};
        } else {
          throw new Error("initialize(): Unknown portType");
        }

        /* This does not work, triggering updatePortAttributes
         * multiple times seems to break stuff horribly.
         if (brokenPorts[portDesc.id]) {
         attrs[portBodySelector].fill = '#F00';
         }
         */

        attrs[portLabelSelector].text = portDesc.proposition;
        if (!isPrototype) {
          attrs[portLabelSelector].display = 'none';
        }
      });
    });

    // Silently set `attrs` on the cell so that noone knows the attrs have changed. This makes sure
    // that, for example, command manager does not register `change:attrs` command but only
    // the important `change:inPorts`/`change:outPorts` command.
    this.attr(attrs, {silent: true});

    // Manually call the `processPorts()` method that is normally called on `change:attrs` (that we just made silent).
    this.processPorts();
    // Let the outside world (mainly the `ModelView`) know that we're done configuring the `attrs` object.
    this.trigger('process:ports');

    // Call the `initialize()` of the parent.
    this.constructor.__super__.constructor.__super__.initialize.apply(this, arguments);
  }

});


joint.shapes.incredible.GenericView = joint.dia.ElementView.extend({

  initialize: function () {

    // `Model` emits the `process:ports` whenever it's done configuring the `attrs` object for ports.
    this.listenTo(this.model, 'process:ports', this.update);

    joint.dia.ElementView.prototype.initialize.apply(this, arguments);
  },

  update: function () {

    // First render ports so that `attrs` can be applied to those newly created DOM elements
    // in `ElementView.prototype.update()`.
    this.renderPorts();
    joint.dia.ElementView.prototype.update.apply(this, arguments);
  },

  renderPorts: function () {

    var $ports = this.$('.ports').empty();

    var portTemplate = _.template(this.model.portMarkup);

    _.each(this.model.ports, function (port, index) {
      $ports.append(V(portTemplate({id: index, port: port})).node);
    });
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

