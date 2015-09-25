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
             consumedBy: v.consumedBy, // may be undefined
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
    }],
  };

  return {
    desc: { label: "✎"+proposition },
    portsGroup: portsGroup,
    canRemove: true
  };
}


// Renders the content of the block (usually label, but might be something fancy)
// and returns its width and height. The latter is used to resize the frame,
// and hence to position the ports.
function renderDesc(desc, group) {
  var text;
  if (desc.label) {
    text = V("<text class='label center' font-family='sans' fill='black'/>");
    text.text(desc.label);
  } else if (desc.intro) {
    text = V("<text class='label right' font-family='sans' fill='black'/>");
    text.text(desc.intro);
  } else if (desc.elim) {
    text = V("<text class='label left' font-family='sans' fill='black'/>");
    text.text(desc.elim);
  } else {
    throw Error("renderDesc: Unknown label desc " + JSON.stringify(desc));
  }
  group.append(text);
}


function looksLikeSchieblehre1(blockDesc) {
  return (blockDesc.portsGroup['local hypothesis']||[]).length == 1;
}
function looksLikeSchieblehre2(blockDesc) {
  return (blockDesc.portsGroup['local hypothesis']||[]).length == 2;
}

function pathDataSchieblehre1(opts) {
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
}

function pathDataSchieblehre2(opts) {
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
}

function renderSchieblehre(group, blockDesc, forReal) {
  var handlerLeft = V("<rect class='resize-left' opacity='0' event='resize-left'/>");
  var handlerRight = V("<rect class='resize-right' opacity='0' event='resize-right'/>");
  group.append(handlerLeft);
  group.append(handlerRight);
  group.prepend(V("<path class='body' fill='#ecf0f1'  stroke='#bdc3c7' stroke-opacity='0.5'/>"));

  _.each(blockDesc.portsGroup, function (thesePorts, portType) {
    _.each(thesePorts, function (portDesc, index) {
      var direction = ({assumption: 'in', conclusion: 'out', 'local hypothesis': 'out'})[portType];
      var orientation = ({assumption: 'left', conclusion: 'right', 'local hypothesis': 'right'})[portType];
      addPort(group, portDesc, direction, orientation, forReal, blockDesc.isPrototype);
    });
  });
}


function updateSizesSchieblehre1(el, blockDesc) {
  var defaultSchieblehreWidth = (blockDesc.isPrototype ? 0 : 40);

  // Minimum sizes
  var impIConfig = {
    leftWidth: 10,
    innerWidthLeft:  20,
    innerWidthRight: 20,
    rightWidth: 10,
    leftHeight: 30,
    rightHeight: 30,
  };

  if (blockDesc.schieblehrewidth === undefined) {
    impIConfig.innerWidthRight += defaultSchieblehreWidth;
  } else {
    impIConfig.innerWidthRight += blockDesc.schieblehrewidth;
  }

  impIConfig.leftHeight = Math.max(impIConfig.leftHeight,
    (blockDesc.portsGroup.assumption.length - 2) * 20 + 10);
      // - 2 as one assumption belongs to the local hypothesis
  impIConfig.rightHeight = Math.max(impIConfig.rightHeight,
    (blockDesc.portsGroup.conclusion.length - 1) * 20 + 10);

  // Get label size and position
  var text = el.findOne(".label");
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

  el.findOne("path.body").attr('d', pathDataSchieblehre1(impIConfig));
  if (!blockDesc.isPrototype) {
    el.findOne("rect.resize-left")
      .attr('transform','')
      .attr({width: impIConfig.leftWidth, height: impIConfig.leftHeight})
      .translate(-impIConfig.innerWidthLeft - impIConfig.leftWidth,
                 -impIConfig.leftHeight + 15);
    el.findOne("rect.resize-right")
      .attr('transform','')
      .attr({width: impIConfig.rightWidth, height: impIConfig.rightHeight})
      .translate( impIConfig.innerWidthRight,
                 -impIConfig.rightHeight + 15);
  }

  // The single hypothesis
  var hyp = blockDesc.portsGroup['local hypothesis'][0];
  el.findOne(".port-wrap-" + hyp.id)
    .attr('transform','')
    .translate(-impIConfig.innerWidthLeft, -10);

  // Find the conclusion for the single as for that
  var localAssumption =
    _.find(blockDesc.portsGroup.assumption,
             function (pd) {return pd.id == hyp.consumedBy;});
  var otherAssumptions =
    _.filter(blockDesc.portsGroup.assumption,
             function (pd) {return pd.id != hyp.consumedBy;});

  el.findOne(".port-wrap-" + localAssumption.id)
    .attr('transform','')
    .translate(impIConfig.innerWidthRight, -10);

  var base;
  base = otherAssumptions.length > 1 ? 10 : 0;
  _.each(otherAssumptions, function (portDesc, index) {
    el.findOne(".port-wrap-" + portDesc.id)
      .attr('transform','')
      .translate(-impIConfig.innerWidthLeft - impIConfig.leftWidth,
        base - 20 * index);
  });
  base = (blockDesc.portsGroup.conclusion.length > 1) ? 10 : 0;
  _.each(blockDesc.portsGroup.conclusion, function (portDesc, index) {
    el.findOne(".port-wrap-" + portDesc.id)
      .attr('transform','')
      .translate(impIConfig.innerWidthRight + impIConfig.rightWidth,
        base - 20 * index);
  });

  if (blockDesc.number) {
    number = el.findOne(".number");
    var bb = number.bbox(true);
    number
      .attr('transform','')
      .translate(impIConfig.innerWidthRight + impIConfig.rightWidth - bb.width - 5, 17.5 - bb.height - 1);
  }

  if (blockDesc.canRemove) {
    el.findOne(".tool-remove")
      .attr('transform','')
      .translate(impIConfig.innerWidthRight + impIConfig.rightWidth,
                -(impIConfig.rightHeight -15 + 3.5 +5));
  }

  // Make sure these asymmetric blocks are still nicely aligned in the logic view
  if (blockDesc.isPrototype) {
    el.findOne(".block").translate((impIConfig.leftWidth - impIConfig.rightWidth)/2);
  }
}

function updateSizesSchieblehre2(el, blockDesc) {
  var defaultSchieblehreWidth = (blockDesc.isPrototype ? 0 : 40);

  // Minimum sizes
  var impIConfig = {
    leftWidth: 10,
    innerWidthLeft:  20,
    innerWidthRight: 20,
    rightWidth: 10,
    leftHeight: 30,
    rightHeight: 30,
  };

  if (blockDesc.schieblehrewidth === undefined) {
    impIConfig.innerWidthRight += defaultSchieblehreWidth;
  } else {
    impIConfig.innerWidthRight += blockDesc.schieblehrewidth;
  }

  impIConfig.leftHeight = Math.max(impIConfig.leftHeight,
    (blockDesc.portsGroup.assumption.length - 3) * 20 + 10);
      // - 3 as two assumptions belong to the local hypotheses
  impIConfig.rightHeight = Math.max(impIConfig.rightHeight,
    (blockDesc.portsGroup.conclusion.length - 1) * 20 + 10);

  // Get label size and position
  var text = el.findOne(".label");
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

  el.findOne("path.body").attr('d', pathDataSchieblehre2(impIConfig));

  if (!blockDesc.isPrototype) {
    el.findOne("rect.resize-left")
      .attr('transform','')
      .attr({width: impIConfig.leftWidth, height: impIConfig.leftHeight})
      .translate(-impIConfig.innerWidthLeft - impIConfig.leftWidth,
                 -impIConfig.leftHeight/2);
    el.findOne("rect.resize-right")
      .attr('transform','')
      .attr({width: impIConfig.rightWidth, height: impIConfig.rightHeight})
      .translate( impIConfig.innerWidthRight,
                 -impIConfig.rightHeight/2);
  }

  // The first hypothesis
  var hyp1 = blockDesc.portsGroup['local hypothesis'][0];
  el.findOne(".port-wrap-" + hyp1.id)
    .attr('transform','')
    .translate(-impIConfig.innerWidthLeft, -10);
  // The second hypothesis
  var hyp2 = blockDesc.portsGroup['local hypothesis'][1];
  el.findOne(".port-wrap-" + hyp2.id)
    .attr('transform','')
    .translate(-impIConfig.innerWidthLeft, 10);

  // Find the conclusion for the single as for that
  var localAssumption1 =
    _.find(blockDesc.portsGroup.assumption,
             function (pd) {return pd.id == hyp1.consumedBy;});
  var localAssumption2 =
    _.find(blockDesc.portsGroup.assumption,
             function (pd) {return pd.id == hyp2.consumedBy;});
  var otherAssumptions =
    _.filter(blockDesc.portsGroup.assumption,
             function (pd) {return pd.id != hyp1.consumedBy && pd.id != hyp2.consumedBy;});

  el.findOne(".port-wrap-" + localAssumption1.id)
    .attr('transform','')
    .translate(impIConfig.innerWidthRight, -10);

  el.findOne(".port-wrap-" + localAssumption2.id)
    .attr('transform','')
    .translate(impIConfig.innerWidthRight, 10);

  var base;
  base = 10 * (otherAssumptions.length-1);
  _.each(otherAssumptions, function (portDesc, index) {
    el.findOne(".port-wrap-" + portDesc.id)
      .attr('transform','')
      .translate(-impIConfig.innerWidthLeft - impIConfig.leftWidth,
        base - 20 * index);
  });
  base = 10 * (blockDesc.portsGroup.conclusion.length - 1);
  _.each(blockDesc.portsGroup.conclusion, function (portDesc, index) {
    el.findOne(".port-wrap-" + portDesc.id)
      .attr('transform','')
      .translate(impIConfig.innerWidthRight + impIConfig.rightWidth,
        base - 20 * index);
  });

  if (blockDesc.number) {
    number = el.findOne(".number");
    var bb = number.bbox(true);
    number
      .attr('transform','')
      .translate(impIConfig.innerWidthRight + impIConfig.rightWidth - bb.width - 5, 17.5 - bb.height - 1);
  }

  if (blockDesc.canRemove) {
    el.findOne(".tool-remove")
      .attr('transform','')
      .translate(impIConfig.innerWidthRight + impIConfig.rightWidth,
                -(impIConfig.rightHeight/2 + 3.5 +5));
  }

  // Make sure these asymmetric blocks are still nicely aligned in the logic view
  if (blockDesc.isPrototype) {
    el.findOne(".block").translate((impIConfig.leftWidth - impIConfig.rightWidth)/2);
  }
}


function renderRegularBlock(group, blockDesc, forReal) {
  var rect = V("<rect class='body' fill='#ecf0f1' rx='5' ry='5' stroke='#bdc3c7' stroke-opacity='0.5'/>");
  group.prepend(rect);
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

  if (blockDesc.number) {
    var numberLabel = V('<text class="number"></text>');
    numberLabel.text(blockDesc.number.toString());
    group.append(numberLabel);
  }

  renderDesc(blockDesc.desc, group);

  // Some special cases
  if (looksLikeSchieblehre1(blockDesc)) {
    renderSchieblehre(group, blockDesc, forReal);
  } else if (looksLikeSchieblehre2(blockDesc)) {
    renderSchieblehre(group, blockDesc, forReal);
  } else {
    renderRegularBlock(group, blockDesc, forReal);
  }

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


  updateSizes(el, blockDesc);
}


function updateSizes(el, blockDesc) {
  // Some special cases
  if (looksLikeSchieblehre1(blockDesc)) {
    updateSizesSchieblehre1(el, blockDesc);
  } else if (looksLikeSchieblehre2(blockDesc)) {
    updateSizesSchieblehre2(el, blockDesc);
  } else {
    updateSizesRegular(el, blockDesc);
  }
}

function updateSizesRegular(el, blockDesc) {
  // Get label size and position
  var text = el.findOne(".label");
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
  width = Math.ceil(width / 10) * 10;

  el.findOne("rect.body")
    .attr({width: width, height: height})
    .attr('transform','')
    .translate(-width/2,-height/2);

  if (blockDesc.number) {
    // lower right corner
    number = el.findOne(".number");
    var bb = number.bbox(true);
    number
      .attr('transform','')
      .translate(- bb.width + width/2 - 5, -bb.height + height/2 - 1);
  }

  if (blockDesc.canRemove) {
    el.findOne(".tool-remove")
      .attr('transform','')
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
      el.findOne(".port-wrap-" + portDesc.id)
        .attr('transform','')
        .translate(pos.x, pos.y);
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

  var label;
  var labelBB;
  if (isPrototype) {
    label = V("<text font-family='sans' fill='#000' font-size='8px'/>");
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
