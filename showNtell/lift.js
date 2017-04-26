function sinAndCos(data) {
    var lift = [];
    var cumLift = [];
    var resp = [];
    resp.push({x:0, y:null})
    cumLift.push({x:0, y:null})
    lift.push({x:0, y:null})
    for (var i = 0; i < 10; i++) {
        lift.push({x: i+1, y: data[i].lift});
        cumLift.push({x: i+1, y: data[i].cumLift});
        resp.push({x: i+1, y: data[i].responseRate});
    }

    cumLift.push({x:data.length+1, y:null})
    lift.push({x:data.length+1, y:null})
    resp.push({x:data.length+1, y:null})
    return [
        {
            area: false,
            values: lift,
            key: "Lift",
            color: "#ff7f0e",
            strokeWidth: 3.5
        },{
            values: cumLift,
            key: "Cummulative Lift",
            color: "#2222ff",
            strokeWidth: 3.5
        },{
            values: resp,
            key: "Response Rate",
            color: "#667711",
            strokeWidth: 3.5
        },

    ];
}

fetch("http://ge-lab.org:8000/lift")
    .then((resp) => resp.json()) // Transform the data into json
    .then(function(dataR) {
      // Wrapping in nv.addGraph allows for '0 timeout render', stores rendered charts in nv.graphs, and may do more in the future... it's NOT required
      var chart;
      var data;
      nv.addGraph(function() {
          chart = nv.models.lineChart()
              .options({
                  duration: 300,
                  useInteractiveGuideline: true
              })
              .forceY([-1,4]);
          chart.xAxis
              .axisLabel("Percentile (%)")
              .tickFormat(d3.format(',.1f'))
              .staggerLabels(true);

          chart.yAxis
              .axisLabel('lift (v)')
              .tickFormat(function(d) {
                  if (d == null) {
                      return 'N/A';
                  }
                  return d3.format(',.2f')(d);
              });
          data = sinAndCos(dataR);
          d3.select('#chart1').append('svg')
              .datum(data)
              .call(chart);
          nv.utils.windowResize(chart.update);
          return chart;
      });
    })
