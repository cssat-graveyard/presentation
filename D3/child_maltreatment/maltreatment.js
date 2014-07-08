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
    startYear = 2003,
    endYear = 2013,
    startRate = 0,
    endRate = 35,
    y = d3.scale.linear().domain([endRate, startRate]).range([0 + margin, h - margin]),
    x = d3.scale.linear().domain([2003, 2013]).range([0 + margin - 5, w]),
    years = d3.range(startYear, endYear);
var vis = d3.select("#vis").append("svg:svg").attr("width", w).attr("height", h).append("svg:g");
var line = d3.svg.line().x(function (d, i) {
    return x(d.x);
}).y(function (d) {
    return y(d.y);
});
var states_regions = {};
d3.text('state-regions.csv', 'text/csv', function (text) {
    var regions = d3.csv.parseRows(text);
    for (i = 1; i < regions.length; i++) {
        states_regions[regions[i][0]] = regions[i][1];
    }
});
var startEnd = {}, stateCodes = {};
d3.text('maltreatment-for-d3.csv', 'text/csv', function (text) {
    var states = d3.csv.parseRows(text);
    for (i = 1; i < states.length; i++) {
        var values = states[i].slice(2, states[i.length - 1]);
        var currData = [];
        stateCodes[states[i][1]] = states[i][0];
        var started = false;
        for (j = 0; j < values.length; j++) {
            if (values[j] != '') {
                currData.push({
                    x: years[j],
                    y: values[j]
                });
                if (!started) {
                    startEnd[states[i][1]] = {
                        'startYear': years[j],
                        'startVal': values[j]
                    };
                    started = true;
                } else if (j == values.length - 1) {
                    startEnd[states[i][1]]['endYear'] = years[j];
                    startEnd[states[i][1]]['endVal'] = values[j];
                }
            }
        }
        vis.append("svg:path").data([currData]).attr("state", states[i][1]).attr("class", states_regions[states[i][1]]).attr("d", line).on("mouseover", onmouseover).on("mouseout", onmouseout);
    }
});
vis.append("svg:line").attr("x1", x(2003)).attr("y1", y(startRate)).attr("x2", x(2012)).attr("y2", y(startRate)).attr("class", "axis")
vis.append("svg:line").attr("x1", x(startYear)).attr("y1", y(startRate)).attr("x2", x(startYear)).attr("y2", y(endRate)).attr("class", "axis")
vis.selectAll(".xLabel").data(x.ticks(5)).enter().append("svg:text").attr("class", "xLabel").text(String).attr("x", function (d) {
    return x(d)
}).attr("y", h - 10).attr("text-anchor", "middle")
vis.selectAll(".yLabel").data(y.ticks(4)).enter().append("svg:text").attr("class", "yLabel").text(String).attr("x", 0).attr("y", function (d) {
    return y(d)
}).attr("text-anchor", "right").attr("dy", 3)
vis.selectAll(".xTicks").data(x.ticks(5)).enter().append("svg:line").attr("class", "xTicks").attr("x1", function (d) {
    return x(d);
}).attr("y1", y(startRate)).attr("x2", function (d) {
    return x(d);
}).attr("y2", y(startRate) + 7)
vis.selectAll(".yTicks").data(y.ticks(4)).enter().append("svg:line").attr("class", "yTicks").attr("y1", function (d) {
    return y(d);
}).attr("x1", x(2003)).attr("y2", function (d) {
    return y(d);
}).attr("x2", x(2003))

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
    var stateVals = startEnd[stateCode];
    var percentChange = 100 * (stateVals['endVal'] - stateVals['startVal']) / stateVals['startVal'];
    var blurb = '<h2>' + stateCodes[stateCode] + '</h2>';
    blurb += "<p>" + stateCodes[stateCode] + " had a rate of " + Math.round(stateVals['startVal']) + " cases per 1,000 in " + stateVals['startYear'] + " and " + Math.round(stateVals['endVal']) + " per 1,000 in " + stateVals['endYear'] + ", ";
    if (percentChange >= 0) {
        blurb += "an increase of " + Math.round(percentChange) + " percent."
    } else {
        blurb += "a decrease of " + -1 * Math.round(percentChange) + " percent."
    }
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