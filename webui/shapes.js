
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

    markup: '<g class="rotatable"><g class="scalable"><rect class="body"/></g><text class="label"/><g class="inPorts"/><g class="outPorts"/><g class="bottomPorts"/></g>',
    portMarkup: '<g class="port port<%= id %>"><circle class="port-body"/><text class="port-label"/></g>',

    defaults: joint.util.deepSupplement({

        type: 'incredible.Generic',
        size: { width: 1, height: 1 },

        inPorts: [],
        outPorts: [],
        bottomPorts: [],

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
            '.inPorts .port-label': { x:-15, dy: 0, 'text-anchor': 'end', fill: '#000000' },
            '.outPorts .port-label':{ x: 15, dy: 0, fill: '#000000' },
            '.bottomPorts .port-label':{ dx: 0, dy: 15, fill: '#000000' }
        }

    }, joint.shapes.basic.Generic.prototype.defaults),


    initialize: function() {

        this.updatePortsAttrs();
        this.on('change:inPorts change:outPorts', this.updatePortsAttrs, this);

        // Call the `initialize()` of the parent.
        this.constructor.__super__.constructor.__super__.initialize.apply(this, arguments);
    },

    updatePortsAttrs: function(eventName) {

        // Delete previously set attributes for ports.
        var currAttrs = this.get('attrs');
        _.each(this._portSelectors, function(selector) {
            if (currAttrs[selector]) delete currAttrs[selector];
        });

        // This holds keys to the `attrs` object for all the port specific attribute that
        // we set in this method. This is necessary in order to remove previously set
        // attributes for previous ports.
        this._portSelectors = [];

        var attrs = {};

        _.each(this.get('inPorts'), function(portName, index, ports) {
            var portAttributes = this.getPortAttrs(portName, index, ports.length, '.inPorts', 'in');
            this._portSelectors = this._portSelectors.concat(_.keys(portAttributes));
            _.extend(attrs, portAttributes);
        }, this);

        _.each(this.get('outPorts'), function(portName, index, ports) {
            var portAttributes = this.getPortAttrs(portName, index, ports.length, '.outPorts', 'out');
            this._portSelectors = this._portSelectors.concat(_.keys(portAttributes));
            _.extend(attrs, portAttributes);
        }, this);

        _.each(this.get('bottomPorts'), function(portName, index, ports) {
            var portAttributes = this.getPortAttrs(portName, index, ports.length, '.bottomPorts', 'out');
            this._portSelectors = this._portSelectors.concat(_.keys(portAttributes));
            _.extend(attrs, portAttributes);
        }, this);

        console.log(attrs);

        // Silently set `attrs` on the cell so that noone knows the attrs have changed. This makes sure
        // that, for example, command manager does not register `change:attrs` command but only
        // the important `change:inPorts`/`change:outPorts` command.
        this.attr(attrs, { silent: true });
        // Manually call the `processPorts()` method that is normally called on `change:attrs` (that we just made silent).
        this.processPorts();
        // Let the outside world (mainly the `ModelView`) know that we're done configuring the `attrs` object.
        this.trigger('process:ports');
    },

    getPortAttrs: function(portSpec, index, total, selector, type) {

        var attrs = {};

        var portClass = 'port' + index;
        var portSelector = selector + '>.' + portClass;
        var portLabelSelector = portSelector + '>.port-label';
        var portBodySelector = portSelector + '>.port-body';

        attrs[portLabelSelector] = { text: portSpec.text };
        attrs[portBodySelector] = { port: { id: portSpec.id || _.uniqueId(type) , type: type } };
        attrs[portSelector] = { ref: '.body' };

        if (selector === '.inPorts') {
            attrs[portSelector]['ref-y'] =  (index + 0.5) * (1 / total);
            attrs[portBodySelector].port.edge = 'left';
        } else if (selector === '.outPorts'){
            attrs[portSelector]['ref-y'] =  (index + 0.5) * (1 / total);
            attrs[portSelector]['ref-dx'] = 1;
            attrs[portBodySelector].port.edge = 'right';
        } else if (selector === '.bottomPorts') {
            attrs[portSelector]['ref-x'] =  (index + 0.5) * (1 / total);
            attrs[portSelector]['ref-dy'] = 1;
            attrs[portBodySelector].port.edge = 'bottom';
	} else {
            throw new Error("getPortAttrs(): Unknown selector");
        }

        return attrs;
    },

    getPortSelector: function(name) {

        var selector = '.inPorts';
        var index = this.get('inPorts').indexOf(name);

        if (index < 0) {
            selector = '.outPorts';
            index = this.get('outPorts').indexOf(name);

            if (index < 0) {
                selector = '.bottomPorts';
                index = this.get('bottomPorts').indexOf(name);

                if (index < 0) throw new Error("getPortSelector(): Port doesn't exist.");
            }
        }

        return selector + '>g:nth-child(' + (index + 1) + ')>circle';
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

        var $inPorts = this.$('.inPorts').empty();
        var $outPorts = this.$('.outPorts').empty();
        var $bottomPorts = this.$('.bottomPorts').empty();

        var portTemplate = _.template(this.model.portMarkup);
        console.log(this.model.ports);

        _.each(_.filter(this.model.ports, function(p) { return p.edge === 'left'; }), function(port, index) {

            $inPorts.append(V(portTemplate({ id: index, port: port })).node);
        });
        _.each(_.filter(this.model.ports, function(p) { return p.edge === 'right'; }), function(port, index) {

            $outPorts.append(V(portTemplate({ id: index, port: port })).node);
        });
        _.each(_.filter(this.model.ports, function(p) { return p.edge === 'bottom'; }), function(port, index) {

            $bottomPorts.append(V(portTemplate({ id: index, port: port })).node);
        });
    }
});



shapes.impI = joint.shapes.incredible.Generic.extend({
    defaults: joint.util.deepSupplement({
            rule: 'impI',
            inPorts: [
                {text: 'B', id: 'in'},
            ],
            outPorts: [
                {text: 'imp(A,B)', id: 'out'},
            ],
            bottomPorts: [
                {text: 'A', id: 'hyp'},
            ],
            size: { width: 80, height: 50 },
            attrs: { '.label': {text: 'impI'}},
    }, joint.shapes.incredible.Generic.prototype.defaults),
});

shapes.impE = joint.shapes.incredible.Generic.extend({
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
    }, joint.shapes.incredible.Generic.prototype.defaults),
});

shapes.conjE = joint.shapes.incredible.Generic.extend({
    defaults: joint.util.deepSupplement({
            rule: 'conjE',
            inPorts: [
                {text: 'and(A,B)', id: 'in'},
            ],
            outPorts: [
                {text: 'A', id: 'out1'},
                {text: 'B', id: 'out2'},
            ],
            size: { width: 80, height: 50 },
            attrs: { '.label': {text: 'conjE'}},
    }, joint.shapes.incredible.Generic.prototype.defaults),
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

