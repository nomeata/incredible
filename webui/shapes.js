function ruleToBlockDesc(rule) {
  var ports = rule.ports;
  var portsList = _.sortBy(_.map(ports, function (v, i) {
    return { id: i,
             proposition: v.proposition,
             type: v.type,
           }
  }), 'id');
  var portsGroup = _.groupBy(portsList, "type");

  var desc = rule.desc || { label: rule.id };

  return {
    desc: desc,
    portsGroup: portsGroup,
    canRemove: true
  }
}

function assumptionToBlockDesc(assumption, task) {
  var portsGroup= {'conclusion': [{
    id: 'out'
  }]};

  return {
    desc: {
      label: task.assumptions[assumption-1]
    },
    portsGroup: portsGroup,
    canRemove: false
  }
}

function conclusionToBlockDesc(conclusion, task) {
  var portsGroup= {'assumption': [{
    id: 'in'
  }]};

  return {
    desc: {
      label: task.conclusions[conclusion-1]
    },
    portsGroup: portsGroup,
    canRemove: false
  }
}

function annotationToBlockDesc(proposition) {
  var portsGroup= {
   'assumption': [{
     id: 'in',
     proposition: incredibleFormatTerm(proposition)
    }],
   'conclusion': [{
     id: 'out',
     proposition: incredibleFormatTerm(proposition)
    }],
  };

  return {
    desc: { label: "✎"+proposition },
    portsGroup: portsGroup,
    canRemove: true
  }
}


// Renders the content of the block (usually label, but might be something fancy)
// and returns its width and height. The latter is used to resize the frame,
// and hence to position the ports.
function renderDesc(desc, group) {
  if (desc.label) {
    var text = V("<text class='label' font-family='sans' fill='black'/>");
    text.text(desc.label);
    group.append(text);
    textBB = text.bbox(true);
    // center text
    text.translate(- textBB.width/2, - textBB.height/2);
    return textBB;
  } else if (desc.intro) {
    var text = V("<text class='label' font-family='sans' fill='black'/>");
    text.text(desc.intro);
    group.append(text);
    textBB = text.bbox(true);
    // center text
    text.translate(- textBB.width/2, - textBB.height/2);
    // shove to the right
    text.translate(20, 0);
    return {width: textBB.width + 40, height: textBB.height};
  } else if (desc.elim) {
    var text = V("<text class='label' font-family='sans' fill='black'/>");
    text.text(desc.elim);
    group.append(text);
    textBB = text.bbox(true);
    // center text
    text.translate(- textBB.width/2, - textBB.height/2);
    // shove to the left
    text.translate(-20, 0);
    return {width: textBB.width + 40, height: textBB.height};
  } else {
    throw Error("renderDesc: Unknown label desc " + JSON.stringify(desc));
  }
}


function looksLikeImpI(blockDesc) {
  console.log(blockDesc);
  return
    ((blockDesc.portsGroup['local hypothesis']||[]).length == 1 &&
     (blockDesc.portsGroup['assumption']||[]).length == 1 &&
     (blockDesc.portsGroup['conclusion']||[]).length == 1);
}

function impILikePath(opts) {
  var start = opts.leftWidth + opts.innerWidth/2;
  var h = function(x) { return "h" + x };
  var v = function(x) { return "v" + x };
  var tr = "a 5 5 0 0 1 5 -5";
  var tb = "a 5 5 0 0 1 5 5";
  var tl = "a 5 5 0 0 1 -5 5";
  var tu = "a 5 5 0 0 1 -5 -5";
  return path = [
    "M" + (-start) + " 10", // left edge, vertical center. Not sure why 10 is needed here?
    v(-17.5 - 5),
    tr,
    h(opts.leftWidth-5-5),
    tb,
    v(17.5 - 5),
    v(3.5),
    h(opts.innerWidth),
    v(-3.5),
    v(-(17.5 - 5)),
    tr,
    h(opts.rightWidth -5 - 5),
    tb,
    v(17.5 -5),
    v(17.5 -5),
    tl,
    h(-(opts.rightWidth -5)),
    h(-opts.innerWidth),
    h(-(opts.leftWidth -5)),
    tu,
    v(-17.5 -5),
    "Z"
    ].join(" ");
}

function renderImpILikeBlock(group, blockDesc, forReal) {
  pathV = V("<path class='body' fill='#ecf0f1'  stroke='#bdc3c7' stroke-opacity='0.5'/>")
  group.append(pathV);

  addPort(group, blockDesc.portsGroup['assumption'][0],
    'in', 'left', forReal, blockDesc.isPrototype);
  addPort(group, blockDesc.portsGroup['local hypothesis'][0],
    'out', 'right', forReal, blockDesc.isPrototype);
  addPort(group, blockDesc.portsGroup['conclusion'][0],
    'out', 'right', forReal, blockDesc.isPrototype);

  /*
  addPort(group, blockDesc.portsGroup['assumption'][0],
    {x:impIConfig.innerWidth/2, y:-10}, 'in', 'left', forReal, blockDesc.isPrototype);
  addPort(group, blockDesc.portsGroup['local hypothesis'][0],
    {x:-impIConfig.innerWidth/2, y:-10}, 'out', 'right', forReal, blockDesc.isPrototype);
  addPort(group, blockDesc.portsGroup['conclusion'][0],
    {x:impIConfig.innerWidth/2 + impIConfig.rightWidth, y:0}, 'out', 'right', forReal, blockDesc.isPrototype);
  */
}

function updateSizesImpI(el, blockDesc) {
  impIConfig = {
    leftWidth: 10,
    innerWidth: 50,
    rightWidth: 40
  };
  el.findOne("path.body").attr('d', impILikePath(impIConfig));

  el.findOne(".port-wrap-" + blockDesc.portsGroup['local hypothesis'][0].id)
    .translate(-impIConfig.innerWidth/2, -10);
  el.findOne(".port-wrap-" + blockDesc.portsGroup['assumption'][0].id)
    .translate(impIConfig.innerWidth/2, -10);
  el.findOne(".port-wrap-" + blockDesc.portsGroup['conclusion'][0].id)
    .translate(impIConfig.innerWidth/2 + impIConfig.rightWidth, 0);
}


function renderRegularBlock(group, blockDesc, forReal) {
  var textBB = renderDesc(blockDesc.desc, group);

  var rect = V("<rect class='body' fill='#ecf0f1' rx='5' ry='5' stroke='#bdc3c7' stroke-opacity='0.5'/>");
  group.prepend(rect);

  if (blockDesc.number) {
    var numberLabel = V('<text class="number"></text>');
    numberLabel.text(blockDesc.number.toString());
    group.append(numberLabel);
    bb = numberLabel.bbox(true);
  }

  _.each(blockDesc.portsGroup, function (thesePorts, portType) {
    _.each(thesePorts, function (portDesc, index) {
      var direction = ({assumption: 'in', conclusion: 'out', 'local hypothesis': 'out'})[portType];
      var orientation = ({assumption: 'left', conclusion: 'right', 'local hypothesis': 'bottom'})[portType];
      addPort(group, portDesc, direction, orientation, forReal, blockDesc.isPrototype);
    });
  });
}

function renderBlockDescToSVG(el, blockDesc, forReal) {
  // forReal: Whether this is going to be used on the paper (in which case the
  // magnet attributes are set)

  if (forReal){
    el.attr('magnet',false);
  }

  var group = V("<g class='block'/>");
  el.append(group);

  if (blockDesc.canRemove) {
    markup = [
      '<g class="tool-remove" event="remove">',
      '<circle r="11" />',
      '<path transform="scale(.8) translate(-16, -16)" d="M24.778,21.419 19.276,15.917 24.777,10.415 21.949,7.585 16.447,13.087 10.945,7.585 8.117,10.415 13.618,15.917 8.116,21.419 10.946,24.248 16.447,18.746 21.948,24.248z"/>',
      '<title>Remove element.</title>',
      '</g>',
    ].join('');
    var tool = V(markup);
    group.append(tool);
  }

  // Some special cases
  if (looksLikeImpI(blockDesc)) {
    renderImpILikeBlock(group, blockDesc, forReal);
  } else {
    renderRegularBlock(group, blockDesc, forReal);
  }

  updateSizes(el, blockDesc);
}


function updateSizes(el, blockDesc) {
  // Some special cases
  if (looksLikeImpI(blockDesc)) {
    updateSizesImpI(el, blockDesc);
  } else {
    updateSizesRegular(el, blockDesc);
  }
}

function updateSizesRegular(el, blockDesc) {
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

  el.findOne("rect.body")
    .attr({width: width, height: height})
    .translate(-width/2,-height/2);

  if (blockDesc.number) {
    // lower right corner
    el.findOne(".number")
      .translate(- bb.width + width/2 - 5, -bb.height + height/2 - 1);
  }

  if (blockDesc.canRemove) {
    el.findOne(".tool-remove")
      .translate(width/2 - 20, -height/2);
  }

  _.each(blockDesc.portsGroup, function (thesePorts, portType) {
    var total = _.size(thesePorts);
    _.each(thesePorts, function (portDesc, index) {
      var pos = {};
      if (portType == "assumption") {
        pos.x = -width/2;
        pos.y = 20*index - 10*(total-1);
      } else if (portType == "conclusion") {
        pos.x = width/2;
        pos.y = 20*index - 10*(total-1);
      } else if (portType == "local hypothesis") {
        pos.x = 20*index - 10*(total-1);
        pos.y = height/2;
      }
      el.findOne(".port-wrap-" + portDesc.id).translate(pos.x, pos.y);
    });
  });
}

function addPort(group, portDesc, direction, orientation, forReal, isPrototype) {
  var g = V('<g/>');
  g.attr('class', 'port-wrap-' + portDesc.id);
  group.append(g);
  var pacman = V('<path class="port-body" stroke="none" fill="#777"/>');
  if (forReal) {
    pacman.attr('magnet', 'true');
  }
  pacman.attr({port: portDesc.id, direction: direction, orientation: orientation});
  g.append(pacman);

  if (isPrototype) {
    var label = V("<text font-family='sans' fill='#000' font-size='8px'/>");
    label.text(portDesc.proposition);
    g.append(label);
    var labelBB = label.bbox(true);
  }

  if (direction === "in") {
    pacman.attr({d: "M0,0 l 0 5 a5,5 0 1,1 5,-5 z"});
  } else if (direction === "out") {
    pacman.attr({d: "M-5,-5 l 0 5 a5,5 0 1,0 5,-5 z"});
  }

  var labelPad = 7;
  if (orientation === 'left') {
    pacman.rotate(135);
    if (isPrototype) {
      label.translate( -labelBB.width - labelPad, - labelBB.height/2 );
    }
  } else if (orientation === 'right') {
    pacman.rotate(135);
    if (isPrototype) {
      label.translate( labelPad, - labelBB.height/2 );
    }
  } else if (orientation === 'bottom') {
    pacman.rotate(225);
    if (isPrototype) {
      label.translate(- labelBB.width/2, labelPad);
    }
  } else {
    throw new Error("renderBlockDescToSVG(): Unknown orientation " + orientation);
  }
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
    this.listenTo(this.model, 'change:brokenPorts change:selected change:annotation', this.update);
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
    var number = this.model.get('number');

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
    blockDesc.number = number;

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

    if (this.model.get('selected')) {
      V(this.vel).toggleClass('selected', true);
    } else {
      V(this.vel).toggleClass('selected', false);
    }

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

