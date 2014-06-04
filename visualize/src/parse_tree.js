// based off mbostock’s block #4062045 November 12, 2012
// http://bl.ocks.org/mbostock/4062045

var width = 1400,
height = 1400,
distance = 90,
radius = 10,
char_pixel_ratio = 7;

var max_depth = 100;
var bfs_queue = [];

var color = d3.scale.category20();

var force = d3.layout.force()
  .charge(-120*5)
  .linkDistance(distance)
  .size([width, height]);

var svg = d3.select(".main").append("svg")
  .attr("width", width)
  .attr("height", height);

var json_data;
var data_graph;
var data_global;
var max_depth = 1;

$.getJSON("ptree.json", function(data) {

    data_global = data;

// Find max-depth - used in coloring
    for(var i = 0; i < data.nodes.length; i++)
    {
        var node = data.nodes[i];
        if(node.group > max_depth)
        {
            max_depth = node.group;
        }
    }



    PlotCourse("PDecl 'main' ");
});

function PlotCourse(course_number){

    // plot_graph = data_global;

    ClearSvg();
    test_d3();

}

function ClearSvg()
{
    d3.select("svg")
        .remove();
    svg = d3.select(".main").append("svg")
        .attr("width", width)
        .attr("height", height);
}

var test_d3 = function() {
    force
        .nodes(data_global.nodes)
        .links(data_global.links)
        .start();

    var node = svg.selectAll(".node")
        .data(data_global.nodes)
        .enter().append("g")
        .call(force.drag);

    var link = svg.selectAll(".link")
        .data(data_global.links)
        .enter().append("line")
        .attr("class", "link")
        .style('marker-start', 'url(#start-arrow)')
        .style("stroke-width", function(d) { return Math.sqrt(d.value)*4; });

  node.append("rect")
    .attr("class", "node")
    .attr("width", function(d){
        return char_pixel_ratio*(d.number.length);
    })
    .attr("height", radius * 2)
    .style("fill", function(d) {
        depth_color = Math.round((100*d.group)/max_depth);
        return d3.rgb(depth_color, depth_color,50+depth_color);

}); //Har sat group = depth
    // .style("fill", function(d) { return color(d.group); });
    // .style("stroke", "rgb(0,0,0)")
    // .style("stroke-width", function(d) { return Math.sqrt(d.weight) * 4; });

    node.append("text")
        .attr("class", "label")
        .attr("y", 10)
        .attr("x", function(d) { return d.number.length * 7 / 2; })
        .style("text-anchor", "middle")
        .style("cursor", "pointer")
        .text(function(d) { return d.number; });

    node.on('click', function(d) {
        // prevent clicking when dragging
        if (d3.event.defaultPrevented) return;

        PlotCourse(d.number);
    });

    force.on("tick", function() {
        link.attr("x1", function(d) {

            return d.source.x + (char_pixel_ratio*d.source.number.length)/2;

        })
            .attr("y1", function(d) {
                var height = 24;

                return d.source.y;// + height/2;
 })
            .attr("x2", function(d) {
                return d.target.x + char_pixel_ratio*d.target.number.length/2;

})
      .attr("y2", function(d) {
          var height = 24;
          return d.target.y;// + height/2;
 });

    node.attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; });
  });
};
