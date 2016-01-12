/*
This file contains all the code related creating SVG representations of blocks.
A family of functions (ruleToBlockDesc etc.) converts some other data types
into a common representation, which is then rendered by renderBlockDescToSVG.

This should be independent from jointjs (besides the use of the vectorizer
library V).
*/


function ruleToBlockDesc(rule) {
  var ports = rule.ports;
  var portsList = _.sortBy(_.map(ports, function (v, i) {
    return { id: i,
             proposition: v.proposition,
             type: v.type,
             consumedBy: v.consumedBy // may be undefined
           };
  }), 'id');
  var portsGroup = _.groupBy(portsList, "type");

  var desc = rule.desc || { label: rule.id };

  return {
    desc: desc,
    portsGroup: portsGroup,
    canRemove: true
  };
}

function assumptionToBlockDesc(assumption, task) {
  var portsGroup= {'conclusion': [{
    id: 'out'
  }]};

  return {
    desc: {
      label: task.assumptions[assumption-1]
    },
    roundLeft: true,
    portsGroup: portsGroup,
    canRemove: false
  };
}

function conclusionToBlockDesc(conclusion, task) {
  var portsGroup= {'assumption': [{
    id: 'in'
  }]};

  return {
    desc: {
      label: task.conclusions[conclusion-1]
    },
    roundRight: true,
    portsGroup: portsGroup,
    canRemove: false
  };
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
    }]
  };

  return {
    desc: { label: "✎"+proposition },
    portsGroup: portsGroup,
    canRemove: true
  };
}

// Returns an vectorizer element describing a remove tool
function render_delete_tool(title) {
  var markup = [
    '<g class="tool-remove" event="remove">',
    '<circle r="11" />',
    '<path transform="scale(.8) translate(-16, -16)" d="M24.778,21.419 19.276,15.917 24.777,10.415 21.949,7.585 16.447,13.087 10.945,7.585 8.117,10.415 13.618,15.917 8.116,21.419 10.946,24.248 16.447,18.746 21.948,24.248z"/>',
    '<title></title>',
    '</g>',
  ].join('');
  var tool = V(markup);
  tool.findOne('title').text(i18n.t(title));
  return tool;
}

// Some object oriented foo to handle the various shapes

function BlockDescRenderer(el, blockDesc, forReal) {
  var obj = {
    el: el,
    blockDesc: blockDesc,
    forReal: forReal
  };
  // Choose right class to use here
  var RendererPrototype;
  if (blockDesc.roundLeft){
    RendererPrototype = blockrenderers.RoundLeft;
  } else if (blockDesc.roundRight){
    RendererPrototype = blockrenderers.RoundRight;
  } else if ((blockDesc.portsGroup['local hypothesis']||[]).length == 1) {
    RendererPrototype = blockrenderers.Schieblehre1;
  } else if ((blockDesc.portsGroup['local hypothesis']||[]).length == 2) {
    RendererPrototype = blockrenderers.Schieblehre2;
  } else {
    RendererPrototype = blockrenderers.RegularBlock;
  }
  var renderer = Object.create(RendererPrototype);
  renderer.el = el;
  renderer.blockDesc = blockDesc;
  renderer.forReal = forReal;
  return renderer;
}

blockrenderers = {};
blockrenderers.RoughlyRectangular = Object.create(BlockDescRenderer);
blockrenderers.RegularBlock = Object.create(blockrenderers.RoughlyRectangular);
blockrenderers.RoundLeft = Object.create(blockrenderers.RoughlyRectangular);
blockrenderers.RoundRight = Object.create(blockrenderers.RoughlyRectangular);
blockrenderers.Schieblehre = Object.create(BlockDescRenderer);
blockrenderers.Schieblehre1 = Object.create(blockrenderers.Schieblehre);
blockrenderers.Schieblehre2 = Object.create(blockrenderers.Schieblehre);

/*
 * Common functions, shared by all renderers
 */

// Renders the description of the block (usually a textual label, but might be
// something fancy)
BlockDescRenderer.renderDesc = function (desc) {
  var text;
  if (desc.label) {
    text = V("<text class='label center' />");
    text.text(desc.label);
  } else if (desc.intro) {
    text = V("<text class='label right' />");
    text.text(desc.intro);
  } else if (desc.elim) {
    text = V("<text class='label left' />");
    text.text(desc.elim);
  } else {
    throw Error("renderDesc: Unknown label desc " + JSON.stringify(desc));
  }
  this.group.append(text);
};


// Position a port
BlockDescRenderer.positionPorts = function(portPos) {
  _.each(portPos, function (pos, id) {
    this.el.findOne(".port-wrap-" + id)
      .attr('transform','')
      .translate(pos.x, pos.y);
  }, this);
};

// One main entry method (the other is updateSizes, which is virtual)
BlockDescRenderer.renderToSVG = function() {
  // forReal: Whether this is going to be used on the paper (in which case the
  // magnet attributes are set)

  if (this.forReal){
    this.el.attr('magnet',false);
  }

  this.group = V("<g class='block'/>");
  this.el.append(this.group);

  if (this.blockDesc.number) {
    var numberLabel = V('<text class="number"></text>');
    numberLabel.text(this.blockDesc.number.toString());
    this.group.append(numberLabel);
  }

  this.renderDesc(this.blockDesc.desc);
  this.renderBodyAndPorts();

  if (this.blockDesc.canRemove) {
    this.group.append(render_delete_tool("Remove element"));
  }

  this.updateSizes();
};


BlockDescRenderer.addPort = function (portDesc, direction, orientation) {
  var g = V('<g/>');
  g.attr('class', 'port-wrap-' + portDesc.id);
  this.group.append(g);
  var pacman = V('<path class="port-body" stroke="none" fill="#777"/>');
  if (this.forReal) {
    pacman.attr('magnet', 'true');
  }
  pacman.attr({port: portDesc.id, direction: direction, orientation: orientation});
  g.append(pacman);

  var label;
  var labelBB;
  if (this.blockDesc.isPrototype) {
    label = V("<text font-family='sans' fill='#000' font-size='14px'/>");
    label.text(portDesc.proposition);
    g.append(label);
    labelBB = label.bbox(true);
  }

  if (direction === "in") {
    pacman.attr({d: "M0,0 l 0 5 a5,5 0 1,1 5,-5 z"});
  } else if (direction === "out") {
    pacman.attr({d: "M-5,-5 l 0 5 a5,5 0 1,0 5,-5 z"});
  }

  var labelPad = 7;
  if (orientation === 'left') {
    pacman.rotate(135);
    if (this.blockDesc.isPrototype) {
      label.translate( -labelBB.width - labelPad, - labelBB.height/2 );
    }
  } else if (orientation === 'right') {
    pacman.rotate(135);
    if (this.blockDesc.isPrototype) {
      label.translate( labelPad, - labelBB.height/2 );
    }
  } else if (orientation === 'bottom') {
    pacman.rotate(225);
    if (this.blockDesc.isPrototype) {
      label.translate(- labelBB.width/2, labelPad);
    }
  } else {
    throw new Error("BlockDescRenderer.addPort(): Unknown orientation " + orientation);
  }
};

BlockDescRenderer.positionNumber = function(x,y) {
  if (this.blockDesc.number) {
    // lower right corner
    number = this.el.findOne(".number");
    if (!number) {return;} // Can happen when loading old proofs
    var bb = number.bbox(true);
    number
      .attr('transform','')
      .translate(- bb.width + x, -bb.height + y);
  }
};

BlockDescRenderer.positionRemoveTool = function (x,y) {
  if (this.blockDesc.canRemove) {
    this.el.findOne(".tool-remove")
      .attr('transform','')
      .translate(x,y);
  }
};


/*
 * Roughly rectangular block
 */
blockrenderers.RoughlyRectangular.renderBodyAndPorts = function () {
  this.renderBody();
  _.each(this.blockDesc.portsGroup, function (thesePorts, portType) {
    _.each(thesePorts, function (portDesc, index) {
      var direction = ({assumption: 'in', conclusion: 'out', 'local hypothesis': 'out'})[portType];
      var orientation = ({assumption: 'left', conclusion: 'right', 'local hypothesis': 'bottom'})[portType];
      this.addPort(portDesc, direction, orientation);
    }, this);
  }, this);
};

blockrenderers.RoughlyRectangular.updateSizes = function () {
  // Get label size and position
  var text = this.el.findOne(".label");
  var textBB = text.bbox(true);
  if (text.hasClass("center")) {
    text
      .attr('transform','')
      .translate(- textBB.width/2, - textBB.height/2);
  } else if (text.hasClass("left")) {
    text
      .attr('transform', '')
      .translate(- textBB.width/2, - textBB.height/2)
      .translate(-20, 0);
    textBB.width = textBB.width + 40;
  } else if (text.hasClass("right")) {
    text
      .attr('transform', '')
      .translate(- textBB.width/2, - textBB.height/2)
      .translate(20, 0);
    textBB.width = textBB.width + 40;
  } else {
    throw Error("renderDesc: Unknown label class");
  }

  // Calculate minimum width/height based on number of ports and label length
  var height = 35;
  var width = 80;
  _.each(this.blockDesc.portsGroup, function (thesePorts, portType) {
    var total = _.size(thesePorts);
    if (portType == 'local hypothesis') {
      width = Math.max(width, 20 * total -5);
    } else {
      height = Math.max(height, 20 * total -5);
    }
  });
  width = Math.max(width, textBB.width + 10);
  height = Math.max(height, textBB.height + 10);
  width = Math.ceil(width / 10) * 10;

  this.resizeBody(width, height);

  this.positionNumber(width/2 - 5, height/2 - 1);
  this.positionRemoveTool(width/2-20, -height/2);

  var portPos = {};
  _.each(this.blockDesc.portsGroup, function (thesePorts, portType) {
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
      portPos[portDesc.id] = pos;
    });
  });
  this.positionPorts(portPos);
};

/*
 * Regular square-shaped block
 */

blockrenderers.RegularBlock.renderBody = function () {
  var rect = V("<rect class='body' fill='#ecf0f1' rx='5' ry='5' stroke='#bdc3c7' stroke-opacity='0.5'/>");
  this.group.prepend(rect);
};

blockrenderers.RegularBlock.resizeBody = function (width, height) {
  this.el.findOne("rect.body")
    .attr({width: width, height: height})
    .attr('transform','')
    .translate(-width/2,-height/2);
};

/*
 * Left rounded block
 */

blockrenderers.RoundLeft.renderBody = function () {
  this.group.prepend(V("<path class='body' fill='#ecf0f1'  stroke='#bdc3c7' stroke-opacity='0.5'/>"));
};

blockrenderers.RoundLeft.resizeBody = function (width, height) {
  var h = function(x) { return "h" + x;};
  var v = function(x) { return "v" + x;};
  var r = height/2;
  var tr = ["a",r,r,0,0,1,r,-r].join(" ");
  var tb = "a 5 5 0 0 1 5 5";
  var tl = "a 5 5 0 0 1 -5 5";
  var tu = ["a",r,r,0,0,1,-r,-r].join(" ");
  var d = [
    "M" + (-width/2) + " 0", // left edge, vertical center
    tr,
    h(width - 5 - height/2),
    tb,
    v(height - 5 - 5),
    tl,
    h(-(width - 5 - height/2)),
    tu,
    "Z"
    ].join(" ");
  this.el.findOne("path.body").attr({d:d});
};

/*
 * Right rounded block
 */

blockrenderers.RoundRight.renderBody = function () {
  this.group.prepend(V("<path class='body' fill='#ecf0f1'  stroke='#bdc3c7' stroke-opacity='0.5'/>"));
};

blockrenderers.RoundRight.resizeBody = function (width, height) {
  var h = function(x) { return "h" + x;};
  var v = function(x) { return "v" + x;};
  var r = height/2;
  var tr = "a 5 5 0 0 1 5 -5";
  var tb = ["a",r,r,0,0,1,r,r].join(" ");
  var tl = ["a",r,r,0,0,1,-r,r].join(" ");
  var tu = "a 5 5 0 0 1 -5 -5";
  var d = [
    "M" + (-width/2) + " 0", // left edge, vertical center
    v(-(height/2-5)),
    tr,
    h(width - 5 - height/2 + 5), // extra +5 for the number in the corner
    tb,
    tl,
    h(-(width - 5 - height/2 + 5)),
    tu,
    v(-(height/2-5)),
    "Z"
    ].join(" ");
  this.el.findOne("path.body").attr({d:d});
};


/*
 * Schieblehre Code (common)
 */

blockrenderers.Schieblehre.renderBodyAndPorts = function () {
  var handlerLeft = V("<rect class='resize-left' opacity='0' event='resize-left'/>");
  var handlerRight = V("<rect class='resize-right' opacity='0' event='resize-right'/>");
  this.group.append(handlerLeft);
  this.group.append(handlerRight);
  this.group.prepend(V("<path class='body' fill='#ecf0f1'  stroke='#bdc3c7' stroke-opacity='0.5'/>"));

  _.each(this.blockDesc.portsGroup, function (thesePorts, portType) {
    _.each(thesePorts, function (portDesc, index) {
      var direction = ({assumption: 'in', conclusion: 'out', 'local hypothesis': 'out'})[portType];
      var orientation = ({assumption: 'left', conclusion: 'right', 'local hypothesis': 'right'})[portType];
      this.addPort(portDesc, direction, orientation);
    }, this);
  }, this);
};

/*
 * Schieblehre Code (Variant 1)
 */

blockrenderers.Schieblehre1.updateSizes = function() {
  var defaultSchieblehreWidth = (this.blockDesc.isPrototype ? 0 : 40);

  // Minimum sizes
  var impIConfig = {
    leftWidth: 10,
    innerWidthLeft:  20,
    innerWidthRight: 20,
    rightWidth: 10,
    leftHeight: 30,
    rightHeight: 30
  };

  if (this.blockDesc.schieblehrewidth === undefined) {
    impIConfig.innerWidthRight += defaultSchieblehreWidth;
  } else {
    impIConfig.innerWidthRight += this.blockDesc.schieblehrewidth;
  }

  impIConfig.leftHeight = Math.max(impIConfig.leftHeight,
    (this.blockDesc.portsGroup.assumption.length - 2) * 20 + 10);
      // - 2 as one assumption belongs to the local hypothesis
  impIConfig.rightHeight = Math.max(impIConfig.rightHeight,
    (this.blockDesc.portsGroup.conclusion.length - 1) * 20 + 10);

  // Get label size and position
  var text = this.el.findOne(".label");
  var textBB = text.bbox(true);
  var shift;
  if (text.hasClass("left")) {
    impIConfig.leftWidth = Math.max(impIConfig.leftWidth, textBB.width + 10, 30);
  } else if (text.hasClass("right") || text.hasClass("center")) {
    impIConfig.rightWidth = Math.max(impIConfig.rightWidth, textBB.width + 10, 30);
  } else {
    throw Error("updateSizesImpI: Unknown label class");
  }
  impIConfig.leftWidth = Math.ceil(impIConfig.leftWidth / 10) * 10;
  impIConfig.rightWidth = Math.ceil(impIConfig.rightWidth / 10) * 10;

  if (text.hasClass("left")) {
    text
      .attr('transform','')
      .translate(- textBB.width/2, - textBB.height/2)
      .translate(- impIConfig.innerWidthLeft - impIConfig.leftWidth/2, 0);
  } else if (text.hasClass("right") || text.hasClass("center")) {
    text
      .attr('transform','')
      .translate(- textBB.width/2, - textBB.height/2)
      .translate(impIConfig.innerWidthRight + impIConfig.rightWidth/2, 0);
  } else {
    throw Error("updateSizesImpI: Unknown label class");
  }

  this.el.findOne("path.body").attr('d', this.pathData(impIConfig));
  if (!this.blockDesc.isPrototype) {
    this.el.findOne("rect.resize-left")
      .attr('transform','')
      .attr({width: impIConfig.leftWidth, height: impIConfig.leftHeight})
      .translate(-impIConfig.innerWidthLeft - impIConfig.leftWidth,
                 -impIConfig.leftHeight + 15);
    this.el.findOne("rect.resize-right")
      .attr('transform','')
      .attr({width: impIConfig.rightWidth, height: impIConfig.rightHeight})
      .translate( impIConfig.innerWidthRight,
                 -impIConfig.rightHeight + 15);
  }

  // The single hypothesis
  var hyp = this.blockDesc.portsGroup['local hypothesis'][0];

  // Find the conclusion for the single as for that
  var localAssumption =
    _.find(this.blockDesc.portsGroup.assumption,
             function (pd) {return pd.id == hyp.consumedBy;});
  var otherAssumptions =
    _.filter(this.blockDesc.portsGroup.assumption,
             function (pd) {return pd.id != hyp.consumedBy;});

  // Port positions
  var portPos = {};

  portPos[hyp.id] =
    {x: -impIConfig.innerWidthLeft, y: -10};
  portPos[localAssumption.id] =
    {x: impIConfig.innerWidthRight, y: -10};

  var base;
  base = otherAssumptions.length > 1 ? 10 : 0;
  _.each(otherAssumptions, function (portDesc, index) {
    portPos[portDesc.id] =
      {x: -impIConfig.innerWidthLeft - impIConfig.leftWidth,
       y: base - 20 * index};
  });
  base = (this.blockDesc.portsGroup.conclusion.length > 1) ? 10 : 0;
  _.each(this.blockDesc.portsGroup.conclusion, function (portDesc, index) {
    portPos[portDesc.id] =
      {x: impIConfig.innerWidthRight + impIConfig.rightWidth,
       y: base - 20 * index};
  });
  this.positionPorts(portPos);

  this.positionNumber(impIConfig.innerWidthRight + impIConfig.rightWidth - 5, 17.5 - 1);
  this.positionRemoveTool(
      impIConfig.innerWidthRight + impIConfig.rightWidth + 2.5,
      -(impIConfig.rightHeight/2 + 3.5 + 7.5)
  );

  // Make sure these asymmetric blocks are still nicely aligned in the logic view
  if (this.blockDesc.isPrototype) {
    this.el.findOne(".block").translate((impIConfig.leftWidth - impIConfig.rightWidth)/2);
  }
};

blockrenderers.Schieblehre1.pathData = function (opts) {
  var start = -opts.leftWidth - opts.innerWidthLeft;
  var h = function(x) { return "h" + x;};
  var v = function(x) { return "v" + x;};
  var tr = "a 5 5 0 0 1 5 -5";
  var tb = "a 5 5 0 0 1 5 5";
  var tl = "a 5 5 0 0 1 -5 5";
  var tu = "a 5 5 0 0 1 -5 -5";
  // height above the zero line
  var extraHeightL = opts.leftHeight - 15 + 2.5;
  var extraHeightR = opts.rightHeight - 15 + 2.5;
  return [
    "M" + start + " 0", // left edge, vertical center
    v(-(extraHeightL - 5)),
    tr,
    h(opts.leftWidth-5-5),
    tb,
    v(extraHeightL - 5 - 3.5),
    h(opts.innerWidthLeft),
    h(opts.innerWidthRight),
    v(-(extraHeightR - 5 - 3.5)),
    tr,
    h(opts.rightWidth -5 - 5),
    tb,
    v(extraHeightR -5),
    v(17.5 -5),
    tl,
    h(-(opts.rightWidth -5)),
    h(-opts.innerWidthRight),
    h(-opts.innerWidthLeft),
    h(-(opts.leftWidth -5)),
    tu,
    v(-17.5 -5),
    "Z"
    ].join(" ");
};

/*
 * Schieblehre Code (Variant 2)
 */


blockrenderers.Schieblehre2.updateSizes = function () {
  var defaultSchieblehreWidth = (this.blockDesc.isPrototype ? 0 : 40);

  // Minimum sizes
  var impIConfig = {
    leftWidth: 10,
    innerWidthLeft:  20,
    innerWidthRight: 20,
    rightWidth: 10,
    leftHeight: 30,
    rightHeight: 30
  };

  if (this.blockDesc.schieblehrewidth === undefined) {
    impIConfig.innerWidthRight += defaultSchieblehreWidth;
  } else {
    impIConfig.innerWidthRight += this.blockDesc.schieblehrewidth;
  }

  impIConfig.leftHeight = Math.max(impIConfig.leftHeight,
    (this.blockDesc.portsGroup.assumption.length - 3) * 20 + 10);
      // - 3 as two assumptions belong to the local hypotheses
  impIConfig.rightHeight = Math.max(impIConfig.rightHeight,
    (this.blockDesc.portsGroup.conclusion.length - 1) * 20 + 10);

  // Get label size and position
  var text = this.el.findOne(".label");
  var textBB = text.bbox(true);
  var shift;
  if (text.hasClass("left")) {
    impIConfig.leftWidth = Math.max(impIConfig.leftWidth, textBB.width + 10, 30);
  } else if (text.hasClass("right") || text.hasClass("center")) {
    impIConfig.rightWidth = Math.max(impIConfig.rightWidth, textBB.width + 10, 30);
  } else {
    throw Error("updateSizesImpI: Unknown label class");
  }
  impIConfig.leftWidth = Math.ceil(impIConfig.leftWidth / 10) * 10;
  impIConfig.rightWidth = Math.ceil(impIConfig.rightWidth / 10) * 10;

  if (text.hasClass("left")) {
    text
      .attr('transform','')
      .translate(- textBB.width/2, - textBB.height/2)
      .translate(- impIConfig.innerWidthLeft - impIConfig.leftWidth/2, 0);
  } else if (text.hasClass("right") || text.hasClass("center")) {
    text
      .attr('transform','')
      .translate(- textBB.width/2, - textBB.height/2)
      .translate(impIConfig.innerWidthRight + impIConfig.rightWidth/2, 0);
  } else {
    throw Error("updateSizesImpI: Unknown label class");
  }

  this.el.findOne("path.body").attr('d', this.pathData(impIConfig));

  if (!this.blockDesc.isPrototype) {
    this.el.findOne("rect.resize-left")
      .attr('transform','')
      .attr({width: impIConfig.leftWidth, height: impIConfig.leftHeight})
      .translate(-impIConfig.innerWidthLeft - impIConfig.leftWidth,
                 -impIConfig.leftHeight/2);
    this.el.findOne("rect.resize-right")
      .attr('transform','')
      .attr({width: impIConfig.rightWidth, height: impIConfig.rightHeight})
      .translate( impIConfig.innerWidthRight,
                 -impIConfig.rightHeight/2);
  }

  // The first hypothesis
  var hyp1 = this.blockDesc.portsGroup['local hypothesis'][0];
  // The second hypothesis
  var hyp2 = this.blockDesc.portsGroup['local hypothesis'][1];

  // Find the conclusion for the single as for that
  var localAssumption1 =
    _.find(this.blockDesc.portsGroup.assumption,
             function (pd) {return pd.id == hyp1.consumedBy;});
  var localAssumption2 =
    _.find(this.blockDesc.portsGroup.assumption,
             function (pd) {return pd.id == hyp2.consumedBy;});
  var otherAssumptions =
    _.filter(this.blockDesc.portsGroup.assumption,
             function (pd) {return pd.id != hyp1.consumedBy && pd.id != hyp2.consumedBy;});

  // Port positions
  var portPos = {};

  portPos[hyp1.id] =
    {x: -impIConfig.innerWidthLeft, y: -10};
  portPos[hyp2.id] =
    {x: -impIConfig.innerWidthLeft, y: 10};
  portPos[localAssumption1.id] =
    {x: impIConfig.innerWidthRight, y: -10};
  portPos[localAssumption2.id] =
    {x: impIConfig.innerWidthRight, y: 10};

  var base;
  base = 10 * (otherAssumptions.length-1);
  _.each(otherAssumptions, function (portDesc, index) {
    portPos[portDesc.id] =
      {x: -impIConfig.innerWidthLeft - impIConfig.leftWidth,
       y: base - 20 * index};
  });
  base = 10 * (this.blockDesc.portsGroup.conclusion.length - 1);
  _.each(this.blockDesc.portsGroup.conclusion, function (portDesc, index) {
    portPos[portDesc.id] =
      {x: impIConfig.innerWidthRight + impIConfig.rightWidth,
       y: base - 20 * index};
  });
  this.positionPorts(portPos);


  this.positionNumber(impIConfig.innerWidthRight + impIConfig.rightWidth - 5, (impIConfig.rightHeight + 5)/2 - 1);
  this.positionRemoveTool(
      impIConfig.innerWidthRight + impIConfig.rightWidth + 2.5,
      -(impIConfig.rightHeight/2 + 3.5 + 7.5)
  );

  // Make sure these asymmetric blocks are still nicely aligned in the logic view
  if (this.blockDesc.isPrototype) {
    this.el.findOne(".block").translate((impIConfig.leftWidth - impIConfig.rightWidth)/2);
  }
};

blockrenderers.Schieblehre2.pathData = function (opts) {
  var start = -opts.leftWidth - opts.innerWidthLeft;
  var h = function(x) { return "h" + x;};
  var v = function(x) { return "v" + x;};
  var tr = "a 5 5 0 0 1 5 -5";
  var tb = "a 5 5 0 0 1 5 5";
  var tl = "a 5 5 0 0 1 -5 5";
  var tu = "a 5 5 0 0 1 -5 -5";
  // height above the zero line
  var extraHeightL = opts.leftHeight/2 + 2.5;
  var extraHeightR = opts.rightHeight/2 + 2.5;
  return [
    "M" + start + " 0", // left edge, vertical center
    v(-(extraHeightL - 5)),
    tr,
    h(opts.leftWidth-5-5),
    tb,
    v(extraHeightL - 5 - 3.5),
    h(opts.innerWidthLeft),
    h(opts.innerWidthRight),
    v(-(extraHeightR - 5 - 3.5)),
    tr,
    h(opts.rightWidth -5 - 5),
    tb,
    v(extraHeightR -5),
    v(extraHeightR -5),
    tl,
    h(-(opts.rightWidth -5 - 5)),
    tu,
    v(-(extraHeightR - 5 - 3.5)),
    h(-(opts.innerWidthRight)),
    h(-(opts.innerWidthLeft)),
    v(extraHeightL - 5 - 3.5),
    tl,
    h(-(opts.leftWidth-5-5)),
    tu,
    v(-(extraHeightL - 5)),
    "Z"
    ].join(" ");
};


