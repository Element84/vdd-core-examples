
// This represents the vdd websocket session.
var session = null;
var duration = 750;

// Creates a player control inside the player div. The function returned can be used to 
// set the data of the player and the function handler.
var playerUpdateFn = vdd.player.createPlayerFn($("div#player"), {duration: duration});

// Handles receiving visualization data through WAMP.
function onVizData(topic, eventData) {
  console.log("visualization data received", eventData);
  // Sets the event data and the handler function.
  setNewRoot(eventData.root, eventData.changes);
  // We just give the player a list of indices. -1 indicates showing the original state of the condition
  // tree. 
  playerUpdateFn([-1].concat(d3.range(eventData.changes.length)), playToIndex);
}

// Connect using the WAMP protocol and register callback for visualization data
session = vdd.wamp.connect(onVizData);

// Handle submitting boolean logic
$("a#submit-logic-text").click(function (event) {
  var logicStr = $("textarea")[0].value;
  vdd.wamp.sendData(session, logicStr);
});

//////////////////////////////
// D3 Stuff

var margin = {top: 20, right: 120, bottom: 20, left: 120},
    width = 960 - margin.right - margin.left,
    height = 800 - margin.top - margin.bottom;
    
var i = 0,
    //The root of the tree that's being displayed
    root,
    //An array of the changes to the tree. We can process these changes and animate them within the tree
    changes,
    //The current index within changes that we are displaying. -1 indicates before the first change.
    currChangeIndex = -1;

var tree = d3.layout.tree()
    .size([height, width]);

var diagonal = d3.svg.diagonal();

var svg = d3.select("svg.chart")
    .attr("width", width + margin.right + margin.left)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

function setNewRoot(newRoot, newChanges){
  root = newRoot
  root.x0 = height / 2;
  root.y0 = 0;
  changes = newChanges;
  currChangeIndex = -1;

  //Update the nodesById map
  self.nodesById = {};
  var setNodesById = function(node) {
    self.nodesById[node._id] = node;
    for (var i = 0; node.children && i < node.children.length; i++) {
      setNodesById(node.children[i]);
    }
  }; 
  setNodesById(root);

  update(root);
}

function playToIndex(newIndex) {
  //Animate the changes between currChangeIndex and the new change index
  var reverse = false;
  var changesToPlay = null;
  if (newIndex > currChangeIndex) {
    // Going forward in time
    // Select changes starting after the current index up to and including newIndex
    changesToPlay = changes.slice(currChangeIndex + 1, newIndex + 1);
  }
  else if (newIndex < currChangeIndex) {
    // Going backward in time. Play the reverse
    changesToPlay = changes.slice(newIndex+1, currChangeIndex+1).reverse();
    reverse = true;
  }
  if (changesToPlay) {
    //Update the global curr change index.
    currChangeIndex = newIndex;
    playChanges(changesToPlay, reverse);
  }
}

// Animates the changes to the root. 
// reverse indicates if the changes should be undone instead of played normally.
function playChanges(changesToPlay, reverse) {
  for(var i=0; i<changesToPlay.length; i++){
    var change = changesToPlay[i];

    if (change.type == "node-move"){
      if (reverse) { 
        moveNode(change.id, change.from);
      }
      else {
        moveNode(change.id, change.to);
      }
    }
    else if (change.type == "node-remove"){
      // TODO reverse node removal won't work until we preinitiallize all the nodes by id.
      if(reverse) {
        addNode(change.id, change.from);
      }
      else {
        removeNode(change.id);
      }
    }
    update(root);
  }
}

//Adds a node that was removed back to the root
function addNode(adding, to) {
  nodesById[to].children.push(nodesById[adding]);
}

function moveNode(mover, to) {
  nodesById[to].children.push(nodesById[mover]);
  removeNode(mover);
}

function removeNode(mover) {
  var moverNode = nodesById[mover];
  var children = moverNode.parent.children;
  children.splice(children.indexOf(moverNode), 1);
}


// Handles all the d3 animation and drawing. Taken almost verbatim from a standard d3 example.
function update(source) {

  // Compute the new tree layout.
  var nodes = tree.nodes(root).reverse(),
      links = tree.links(nodes);

  // Normalize for fixed-depth.
  nodes.forEach(function(d) { d.y = d.depth * 180; });

  // Update the nodes…
  var node = svg.selectAll("g.node")
      .data(nodes, function(d) { return d.id || (d.id = ++i); });

  // Enter any new nodes at the parent's previous position.
  var nodeEnter = node.enter().append("g")
      .attr("class", "node")
      .attr("transform", function(d) { return "translate(" + source.x0 + "," + source.y0 + ")"; });

  // TODO change some styling here to be by type
  nodeEnter.append("circle")
      .attr("r", 1e-6)
      .style("fill", function(d) { return "#fff"; });

  nodeEnter.append("text")
      .attr("x", function(d) { return d.children ? -10 : 10; })
      .attr("dy", ".35em")
      .attr("text-anchor", function(d) { return d.children ? "end" : "start"; })
      .text(function(d) { return d.name; })
      .style("fill-opacity", 1e-6);

  // Transition nodes to their new position.
  var nodeUpdate = node.transition()
      .duration(duration)
      .attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; });

  nodeUpdate.select("circle")
      .attr("r", 4.5)
      .style("fill", function(d) { return "#fff"; });

  nodeUpdate.select("text")
      .style("fill-opacity", 1);

  // Transition exiting nodes to the parent's new position.
  var nodeExit = node.exit().transition()
      .duration(duration)
      .attr("transform", function(d) { return "translate(" + source.x + "," + source.y + ")"; })
      .remove();

  nodeExit.select("circle")
      .attr("r", 1e-6);

  nodeExit.select("text")
      .style("fill-opacity", 1e-6);

  // Update the links…
  var link = svg.selectAll("path.link")
      .data(links, function(d) { return d.target.id; });

  // Enter any new links at the parent's previous position.
  link.enter().insert("path", "g")
      .attr("class", "link")
      .attr("d", function(d) {
        var o = {x: source.x0, y: source.y0};
        return diagonal({source: o, target: o});
      });

  // Transition links to their new position.
  link.transition()
      .duration(duration)
      .attr("d", diagonal);

  // Transition exiting nodes to the parent's new position.
  link.exit().transition()
      .duration(duration)
      .attr("d", function(d) {
        var o = {x: source.x, y: source.y};
        return diagonal({source: o, target: o});
      })
      .remove();

  // Stash the old positions for transition.
  nodes.forEach(function(d) {
    d.x0 = d.x;
    d.y0 = d.y;
  });
}
