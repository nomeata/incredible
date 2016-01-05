/*
This file contains our custom elements, and their views.
*/


/*
 * How does jointJS find the view for a model?
 * It takes the 'type' field, of the model, excepts it to be foo.bar
 * and then looks at join.shapes[foo][bar+'View']....
 */


joint.shapes.incredible = {};

/*
 * We avoid the crazy `attrs` feature of JointJS and keep the model as semantic
 * as possible.
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
      this.listenTo(this.model.graph, 'change:qed', this.update);
    }
    this.listenTo(this.model, 'change:schieblehrewidth', this.updateSizes);
    this.listenTo(this.model, 'change:schieblehrewidth', this.updateSizes);
  },

  getBlockDesc: function () {
    // These will not all be defined
    var rule = this.model.get('rule');
    var assumption = this.model.get('assumption');
    var conclusion = this.model.get('conclusion');
    var annotation = this.model.get('annotation');
    var task = this.model.get('task');
    var number = this.model.get('number');

    var blockDesc;
    if (rule) {
      blockDesc = ruleToBlockDesc(rule);
    } else if (assumption) {
      blockDesc = assumptionToBlockDesc(assumption, task);
    } else if (conclusion) {
      blockDesc = conclusionToBlockDesc(conclusion, task);
    } else if (annotation) {
      blockDesc = annotationToBlockDesc(annotation);
    } else {
        throw new Error("renderMarkup(): Unknown block type");
    }
    blockDesc.number = number;

    blockDesc.schieblehrewidth = this.model.get('schieblehrewidth');

    return blockDesc;
  },

  renderMarkup: function() {
    BlockDescRenderer(this.vel, this.getBlockDesc(), true).renderToSVG();

    var cellView = this;
    function resizeSchieblehre(e) {
      var svgPoint = paper.svg.createSVGPoint();
      svgPoint.x = e.pageX;
      svgPoint.y = e.pageY;
      var pos0 = svgPoint.matrixTransform(paper.viewport.getCTM().inverse());
      var action = V(e.target).attr('event');

      document.onmousemove = function(e){
        var svgPoint = paper.svg.createSVGPoint();
        svgPoint.x = e.pageX;
        svgPoint.y = e.pageY;
        var pos1 = svgPoint.matrixTransform(paper.viewport.getCTM().inverse());
        cellView.notify('element:schieblehre', action, pos1.x - pos0.x, pos1.y - pos0.y);
        pos0 = pos1;
      };

      document.onmouseup = function(e){
        document.onmousemove = null;
        document.onmouseup = null;
        cellView.notify('element:schieblehre:ready');
      };
      e.stopPropagation();
    }
    _.each(this.vel.find(".resize-left"), function (vel) {
      vel.node.addEventListener("mousedown", resizeSchieblehre);
    });
    _.each(this.vel.find(".resize-right"), function (vel) {
      vel.node.addEventListener("mousedown", resizeSchieblehre);
    });

    if (this.model.get('conclusion')) {
      V(this.vel.findOne(".block")).addClass('conclusion');
    }
  },

  updateSizes: function () {
    BlockDescRenderer(this.vel, this.getBlockDesc(), true).updateSizes();
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

    if (this.model.get('conclusion')){
      V(this.vel).toggleClass('qed', this.model.graph.get('qed'));
    }

    if (this.model.get('annotation')) {
      var textV = V(this.vel.findOne(".label"));
      var text = "✎"+this.model.get('annotation');
      if (textV.text() != text) {
        textV.text(text);
        this.updateSizes();
      }
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


// This is a work-around for https://github.com/clientIO/joint/issues/178
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
      paddingBox: function () {return {x: -5, y: -5, width: 10, height: 10};},
      startDirections: startDirections,
      endDirections: endDirections
    };
    return manhattan.call(this, vertices, _.extend({},args,opt), linkView);
  };
})();

