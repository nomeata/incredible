function ruleToBlockDesc(rule) {
  var ports = rule.ports;
  var portsList = _.sortBy(_.map(ports, function (v, i) {return _.extend({id: i}, v);}), 'id');
  var portsGroup = _.groupBy(portsList, "type");

  return {
    label: rule.id,
    portsGroup: portsGroup
  }
}

function assumptionToBlockDesc(assumption, task) {
  var portsGroup= {'conclusion': [{
    id: 'out'
  }]};

  return {
    label: task.assumptions[assumption-1],
    portsGroup: portsGroup
  }
}

function conclusionToBlockDesc(conclusion, task) {
  var portsGroup= {'assumption': [{
    id: 'in'
  }]};

  return {
    label: task.conclusions[conclusion-1],
    portsGroup: portsGroup
  }
}

function annotationToBlockDesc(proposition) {
  var portsGroup= {
   'assumption': [{
     id: 'in',
     proposition: proposition
    }],
   'conclusion': [{
     id: 'out',
     proposition: proposition
    }],
  };

  return {
    label: "✎"+proposition,
    portsGroup: portsGroup
  }
}

function renderBlockDescToSVG(el, blockDesc, forReal) {
  // forReal: Whether this is going to be used on the paper (in which case the
  // magnet attributes are set)

  if (forReal){
    el.attr('magnet',false);
  }

  var group = V("<g class='block'/>");
  el.append(group);

  var text = V("<text class='label' font-family='sans' fill='black'/>");
  text.text(blockDesc.label);
  group.append(text);
  textBB = text.bbox(true);
  // center text
  text.translate(- textBB.width/2, - textBB.height/2);

  var rect = V("<rect class='body' fill='#ecf0f1' rx='5' ry='5' stroke='#bdc3c7' stroke-opacity='0.5'/>");

  // Calculate minimum width/height based on number of ports and label length
  var height = 35;
  var width = 80;
  _.each(blockDesc.portsGroup, function (thesePorts, portType) {
    var total = _.size(thesePorts);
    if (portType == 'local hypothesis') {
      width = Math.max(width, 20 * total -5);
    } else {
      height = Math.max(height, 20 * total -5);
    }
  });
  width = Math.max(width, textBB.width + 10);
  height = Math.max(height, textBB.height + 10);

  rect.attr({width: width, height: height});
  rect.translate(-width/2,-height/2);
  group.prepend(rect);


  _.each(blockDesc.portsGroup, function (thesePorts, portType) {
    var total = _.size(thesePorts);
    _.each(thesePorts, function (portDesc, index) {
      var direction = ({assumption: 'in', conclusion: 'out', 'local hypothesis': 'out'})[portType];
      var orientation = ({assumption: 'left', conclusion: 'right', 'local hypothesis': 'bottom'})[portType];
      var pacman = V('<path class="port-body" stroke="none" fill="#777"/>');
      if (forReal) {
        pacman.attr('magnet', 'true');
      }
      pacman.attr({port: portDesc.id, direction: direction, orientation: orientation});

      group.append(pacman);

      if (blockDesc.isPrototype) {
        var label = V("<text font-family='sans' fill='#000' font-size='8px'/>");
        label.text(portDesc.proposition);
        group.append(label);
        var labelBB = label.bbox(true);
      }

      var labelPad = 7;

      if (portType === 'assumption') {
        // put left
        var y = 20*index - 10*(total-1);
        pacman.translate( -width/2, y);
        pacman.rotate(135);
        pacman.attr({d: "M0,0 l 0 5 a5,5 0 1,1 5,-5 z"});
        if (blockDesc.isPrototype) {
          label.translate( -width/2 -labelBB.width - labelPad, y - labelBB.height/2 );
        }
      } else if (portType === 'conclusion') {
        // put right
        var y = 20*index - 10*(total-1);
        pacman.translate( width/2, y);
        pacman.rotate(135);
        pacman.attr({d: "M-5,-5 l 0 5 a5,5 0 1,0 5,-5 z"});
        if (blockDesc.isPrototype) {
          label.translate( width/2 + labelPad, y - labelBB.height/2 );
        }
      } else if (portType === 'local hypothesis') {
        // put below
        var x = 20*index - 10*(total-1);
        pacman.translate( x, height/2);
        pacman.rotate(225);
        pacman.attr({d: "M-5,-5 l 0 5 a5,5 0 1,0 5,-5 z"});
        if (blockDesc.isPrototype) {
          label.translate( x - labelBB.width/2, height/2 + labelPad);
        }
      } else {
        throw new Error("renderBlockDescToSVG(): Unknown portType");
      }
    });
  });
}


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
    this.listenTo(this.model, 'change:brokenPorts change:annotation', this.update);
    if (this.model.get('conclusion')) {
      this.listenTo(this.model, 'change:qed', this.update);
    };
  },

  renderMarkup: function() {
    // These will not all be defined
    var rule = this.model.get('rule');
    var assumption = this.model.get('assumption');
    var conclusion = this.model.get('conclusion');
    var annotation = this.model.get('annotation');
    var task = this.model.get('task');

    if (rule) {
      var blockDesc = ruleToBlockDesc(rule);
    } else if (assumption) {
      var blockDesc = assumptionToBlockDesc(assumption, task);
    } else if (conclusion) {
      var blockDesc = conclusionToBlockDesc(conclusion, task);
    } else if (annotation) {
      var blockDesc = annotationToBlockDesc(annotation);
    } else {
        throw new Error("renderMarkup(): Unknown block type");
    }

    renderBlockDescToSVG(this.vel, blockDesc, true);
  },
  update: function () {
    // Do our own stuff
    var brokenPorts = this.model.get('brokenPorts') || {};

    _.each(this.vel.find(".port-body"), function (port) {
      if (V(port).attr('port') in brokenPorts) {
        V(port).attr('fill', '#F00');
      } else {
        V(port).attr('fill', '#777');
      }
    });

    if (this.model.get('conclusion')) {
      if (this.model.get('qed')) {
        V(this.vel.findOne(".body")).attr('fill','#0f0');
      } else {
        V(this.vel.findOne(".body")).attr('fill','#ecf0f1');
      };
    }

    // This is a bit ugly; we usually set the label in the renderMarkup
    // function, where it also affects the size of the block. How to do it
    // better?
    if (this.model.get('annotation')) {
      var text = V(this.vel.findOne(".label"));
      text.text("✎"+this.model.get('annotation'));
      textBB = text.bbox(true);
      text.attr('transform', '');
      // center text
      text.translate(- textBB.width/2, - textBB.height/2);
    }

    // Just in case we use the attrs feature, let's call the jointjs update function
    joint.dia.ElementView.prototype.update.apply(this, arguments);
  }
});

joint.shapes.incredible.Link = joint.dia.Link.extend({

    arrowheadMarkup: [
        '<g class="marker-arrowhead-group marker-arrowhead-group-<%= end %>">',
        '<circle class="marker-arrowhead" end="<%= end %>" r="7"/>',
        // '<path class="marker-arrowhead" end="<%= end %>" d="M 26 0 L 0 13 L 26 26 z" />',
        '</g>'
    ].join(''),

    vertexMarkup: [
        '<g class="marker-vertex-group" transform="translate(<%= x %>, <%= y %>)">',
        '<circle class="marker-vertex" idx="<%= idx %>" r="10" />',
        '<g class="marker-vertex-remove-group">',
        '<path class="marker-vertex-remove-area" idx="<%= idx %>" d="M16,5.333c-7.732,0-14,4.701-14,10.5c0,1.982,0.741,3.833,2.016,5.414L2,25.667l5.613-1.441c2.339,1.317,5.237,2.107,8.387,2.107c7.732,0,14-4.701,14-10.5C30,10.034,23.732,5.333,16,5.333z" transform="translate(5, -33)"/>',
        '<path class="marker-vertex-remove" idx="<%= idx %>" transform="scale(.8) translate(9.5, -37)" d="M24.778,21.419 19.276,15.917 24.777,10.415 21.949,7.585 16.447,13.087 10.945,7.585 8.117,10.415 13.618,15.917 8.116,21.419 10.946,24.248 16.447,18.746 21.948,24.248z">',
        '<title>Remove vertex.</title>',
        '</path>',
        '</g>',
        '</g>'
    ].join(''),

    defaults: joint.util.deepSupplement({

        type: 'incredible.Link',

        attrs: {
            '.connection': { 'stroke-width': 2 },
            '.marker-vertex': { r: 7 }
        },

        // router: { name: 'orthogonal' },

	/*
        router: { name: 'manhattan', args: {
          paddingBox: function () {return {x: -5, y: -5, width: 10, height: 10}},
          startDirections: ['right'],
          endDirections: ['left']
        }},
	*/
        router: { name: 'wrappedmanhattan' },
        connector: { name: 'rounded', args: { radius: 10 }}

    }, joint.dia.Link.prototype.defaults)

});


joint.routers.wrappedmanhattan = (function (){
  var manhattan = joint.routers.manhattan;

  return function(vertices, opt, linkView) {
    var startDirections = ['right']; // somewhat sensible default
    var endDirections = ['left']; // somewhat sensible default
    if (this.sourceMagnet) {
      startDirections = [this.sourceMagnet.getAttribute('orientation')];
    }
    if (this.targetMagnet) {
      endDirections = [this.targetMagnet.getAttribute('orientation')];
    }

    var args = {
      paddingBox: function () {return {x: -5, y: -5, width: 10, height: 10}},
      startDirections: startDirections,
      endDirections: endDirections
    }
    return manhattan.call(this, vertices, _.extend({},args,opt), linkView);
  };
})();

