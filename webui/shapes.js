
shapes = {



};

// Abstract shapes

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
    portMarkup: '<g class="port port<%= id %>"><circle class="port-body"/><text class="port-label"/></g>',

    defaults: joint.util.deepSupplement({

        type: 'incredible.Generic',
        size: { width: 80, height: 40 },

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
        }

    }, joint.shapes.basic.Generic.prototype.defaults),


    initialize: function() {
        var attrs = {};

        var ruleObj = this.get('rule');

        attrs['.label'] = {text: ruleObj.id};


        var rules = ruleObj.ports;
        var rulesList = _.map(rules, function (v,i) {return _.extend({id:i}, v);});
        var rulesGroup = _.groupBy(rulesList, "type");

        _.each(rulesGroup, function(thesePorts, portType) {
            var total = _.size(thesePorts);
            _.each(thesePorts, function(portDesc, index) {
                var type = ({assumption: 'in', conclusion: 'out', 'local hypothesis': 'out'})[portType];
                var selector = '.ports';

                var portClass = 'port' + portDesc.id;
                var portSelector = selector + '>.' + portClass;
                var portLabelSelector = portSelector + '>.port-label';
                var portBodySelector = portSelector + '>.port-body';

                attrs[portBodySelector] = { port: { id: portDesc.id, type: type } };
                attrs[portSelector] = { ref: '.body' };

                if (portType === 'assumption') {
                    attrs[portSelector]['ref-y'] =  (index + 0.5) * (1 / total);
                    attrs[portBodySelector].port.edge = 'left';
                    attrs[portLabelSelector] = { x:-15, y: 5, 'y-alignment': 'middle', 'text-anchor': 'end', fill: '#000000' };
                } else if (portType === 'conclusion'){
                    attrs[portSelector]['ref-y'] =  (index + 0.5) * (1 / total);
                    attrs[portSelector]['ref-dx'] = 0;
                    attrs[portBodySelector].port.edge = 'right';
                    attrs[portLabelSelector] = { x: 15, y: 5, 'y-alignment': 'middle', fill: '#000000' };
                } else if (portType === 'local hypothesis') {
                    attrs[portSelector]['ref-x'] =  (index + 0.5) * (1 / total);
                    attrs[portSelector]['ref-dy'] = 1;
                    attrs[portBodySelector].port.edge = 'bottom';
                    attrs[portLabelSelector] = { x: 0, y: 20, 'text-anchor': 'middle', fill: '#000000' };
                } else {
                    throw new Error("initialize(): Unknown portType");
                }

                attrs[portLabelSelector].text = portDesc.proposition;
            });
        });


        // Silently set `attrs` on the cell so that noone knows the attrs have changed. This makes sure
        // that, for example, command manager does not register `change:attrs` command but only
        // the important `change:inPorts`/`change:outPorts` command.
        this.attr(attrs, { silent: true });
        // Manually call the `processPorts()` method that is normally called on `change:attrs` (that we just made silent).
        this.processPorts();
        // Let the outside world (mainly the `ModelView`) know that we're done configuring the `attrs` object.
        this.trigger('process:ports');

        // Call the `initialize()` of the parent.
        this.constructor.__super__.constructor.__super__.initialize.apply(this, arguments);
    }

});


joint.shapes.incredible.GenericView = joint.dia.ElementView.extend({

    initialize: function() {

        // `Model` emits the `process:ports` whenever it's done configuring the `attrs` object for ports.
        this.listenTo(this.model, 'process:ports', this.update);

        joint.dia.ElementView.prototype.initialize.apply(this, arguments);
    },

    update: function() {

        // First render ports so that `attrs` can be applied to those newly created DOM elements
        // in `ElementView.prototype.update()`.
        this.renderPorts();
        joint.dia.ElementView.prototype.update.apply(this, arguments);
    },

    renderPorts: function() {

        var $ports = this.$('.ports').empty();

        var portTemplate = _.template(this.model.portMarkup);

        console.log(this.model.ports);
        _.each(this.model.ports,function(port, index) {
            $ports.append(V(portTemplate({ id: index, port: port })).node);
        });
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

