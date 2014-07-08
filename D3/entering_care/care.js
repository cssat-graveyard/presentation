var regions = {
    "FW": "Far West",
    "SW": "Southwest",
    "SE": "Southeast",
    "ME": "Mideast",
    "MT": "Rocky Mountain",
    "GL": "Great Lakes",
    "GP": "Plains",
    "NE": "New England"
},  w = 900,
    h = 476,
    margin = 30,
    startAge = 0,
    endAge = 18,
    startRate = 0,
    endRate = 25,
    y = d3.scale.linear().domain([endRate, startRate]).range([0 + margin, h - margin]),
    x = d3.scale.linear().domain([0, 18]).range([0 + margin - 5, w]),
    Ages = d3.range(startAge, endAge);
var graph = d3.select("#vis").append("svg:svg").attr("width", w).attr("height", h).append("svg:g");
var line = d3.svg.line().x(function (d, i) {
    return x(d.x);
}).y(function (d) {
    return y(d.y);
});
var state_regions = {};
d3.text('state-regions.csv', 'text/csv', function (text) {
    var regions = d3.csv.parseRows(text);
    for (i = 1; i < regions.length; i++) {
        state_regions[regions[i][0]] = regions[i][1];
    }
});
var startAge = {}, stateNames = {};
d3.text('2012-EnteredCare.csv', 'text/csv', function (text) {
    var states = d3.csv.parseRows(text);
    for (i = 1; i < states.length; i++) {
        var values = states[i].slice(4, states[i.length - 1]);
        var currData = [];
        stateNames[states[i][1]] = states[i][0];
        var started = false;
        for (j = 0; j < values.length; j++) {
            if (values[j] != '') {
                currData.push({
                    x: Ages[j],
                    y: values[j]
                });
                if (!started) {
                    startAge[states[i][1]] = {
                        'startAge': Ages[j],
                        'startVal': values[j]
                    };
                    started = true;
                } else if (j == values.length - 1) {
                    startAge[states[i][1]]['endAge'] = Ages[j];
                    startAge[states[i][1]]['endVal'] = values[j];
                }
            }
        }
        graph.append("svg:path").data([currData]).attr("state", states[i][1]).attr("young", states[i][2]).attr("old", states[i][3]).attr("class", state_regions[states[i][1]]).attr("d", line).on("mouseover", onmouseover).on("mouseout", onmouseout);
    }
});
graph.append("svg:line").attr("x1", x(0)).attr("y1", y(startRate)).attr("x2", x(17)).attr("y2", y(startRate)).attr("class", "axis")
graph.append("svg:line").attr("x1", x(0)).attr("y1", y(startRate)).attr("x2", x(0)).attr("y2", y(endRate)).attr("class", "axis")
graph.selectAll(".xLabel").data(x.ticks(5)).enter().append("svg:text").attr("class", "xLabel").text(String).attr("x", function (d) {
    return x(d)
}).attr("y", h - 10).attr("text-anchor", "middle")
graph.selectAll(".yLabel").data(y.ticks(4)).enter().append("svg:text").attr("class", "yLabel").text(String).attr("x", 0).attr("y", function (d) {
    return y(d)
}).attr("text-anchor", "right").attr("dy", 3)
graph.selectAll(".xTicks").data(x.ticks(5)).enter().append("svg:line").attr("class", "xTicks").attr("x1", function (d) {
    return x(d);
}).attr("y1", y(startRate)).attr("x2", function (d) {
    return x(d);
}).attr("y2", y(startRate) + 7)
graph.selectAll(".yTicks").data(y.ticks(4)).enter().append("svg:line").attr("class", "yTicks").attr("y1", function (d) {
    return y(d);
}).attr("x1", x(0)).attr("y2", function (d) {
    return y(d);
}).attr("x2", x(0))

function onclick(d, i) {
    var currClass = d3.select(this).attr("class");
    if (d3.select(this).classed('selected')) {
        d3.select(this).attr("class", currClass.substring(0, currClass.length - 9));
    } else {
        d3.select(this).classed('selected', true);
    }
}

function onmouseover(d, i) {
    var currClass = d3.select(this).attr("class");
    d3.select(this).attr("class", currClass + " current");
    var stateCode = $(this).attr("state");
    var rank0 = $(this).attr("young");
    var rank17 = $(this).attr("old");
    var blurb = '<h2>' + stateNames[stateCode] + '</h2>';
    blurb += "<p>" + stateNames[stateCode] + " ranks #" + rank0 + " for children under 1 entering care and #" + rank17 + " for children age 17.";
    blurb += "</p>";
    $("#default-blurb").hide();
    $("#blurb-content").html(blurb);
}

function onmouseout(d, i) {
    var currClass = d3.select(this).attr("class");
    var prevClass = currClass.substring(0, currClass.length - 8);
    d3.select(this).attr("class", prevClass);
    $("#default-blurb").show();
    $("#blurb-content").html('');
}

function showRegion(regionCode) {
    var states = d3.selectAll("path." + regionCode);
    if (states.classed('highlight')) {
        states.attr("class", regionCode);
    } else {
        states.classed('highlight', true);
    }
}