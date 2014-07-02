 function show(x){
    if((typeof x) == "string"){
        document.write("<pre>" + '"' + x + '"' + "</pre>");
    }else if((typeof x) == "object"){
        document.write("<pre>" + JSON.stringify(x, null, " ")+ "</pre>");
    }else{
        document.write("<pre>" + x + "</pre>");
    }
}

 
    (function(){
        
      d3.population = function(){
        var  population_w = 133,
             population_h = 80,
             padding = 10,
             idz = populationId,
             titles = titles_county;
             statisticz = populationStatistic;
         
        function population(g){
           g.each(function(d, i) {
            
            var id = idz.call(this, d, i),
                peak = titles.population_fast_fact[id].max,
                nadir = titles.population_fast_fact[id].min,
                title = titles.population_fast_fact[id].title, 
                valueFormat = d3.format(".2" + titles.population_fast_fact[id].valueFormat),
                statistic = statisticz.call(this, d, i);
                 
            var g = d3.select(this);
             
            var yScale = d3.scale.linear()
                         .domain([nadir, peak])
                         .range([population_h - padding, padding]);

            var yAxis = d3.svg.axis()
                              .scale(yScale)
                              .orient("left")
                              .ticks(0);      
                   
            var svg = g.append("svg")
                     .attr("width", population_w)
                     .attr("height", population_h)
                     .attr("id",id)
                     .append("g");
            
            svg.append("rect")
                .attr("width",population_w)
                .attr("height",population_h)
                .attr("class","population_rect")
                .style("fill","white")
                .style("opacity",.005);
                       
            var axis = svg.append("g")
                    .attr("class", "population_axis")
                    .attr("transform", "translate(55,0)");
                
            axis.call(yAxis);
            
            axis.append("text")
                .attr("class","text_axis")
                .text(valueFormat(peak))
                .attr("transform", "translate(-10,0)")
                .attr("y", 9)
                .attr("dy", ".71em")
                .style("text-anchor", "end");
                
            axis.append("text")
                .attr("class","text_axis")
                .text(valueFormat(nadir))
                .attr("y", 11)
                .style("text-anchor", "end")
                .attr("transform", "translate(-10," + ( population_h - 2 * padding ) + ")");
                
            svg.append("circle")
                .attr("class","circle_population_background")
                .attr("r",7)
                .attr("cx",55)
                .attr("cy",yScale(nadir))
                .style("fill","white")
                .transition()
                .duration(500)
                .attr("cy",yScale(statistic));
                
            svg.append("circle")
                .attr("class","circle_population")
                .attr("r",5)
                .attr("cx",55)
                .attr("cy",yScale(nadir))
                .transition()
                .duration(500)
                .attr("cy",yScale(statistic));
                    
            svg.append("text")
                .attr("class","text_title")
                .text(title)
                .attr("y", 9)
                .attr("dy", ".71em")
                .style("text-anchor", "middle")
                .attr("transform", "translate(120," + ( population_h / 2 - 40 ) + ")");
                
            svg.append("text")
                .attr("class","text_statis")
                .text(valueFormat(statistic))
                .attr("y", 3)
                .attr("dy", ".71em")
                .style("text-anchor", "middle")
                .attr("transform", "translate(120," + ( population_h / 2 ) + ")");    
             
                  });  
            }        
                  
            population.population_w = function(_) {
                if (!arguments.length) return population_w;
                population_w = _;
                return population;
            }
            
            population.population_h = function(_) {
                if (!arguments.length) return population_h;
                population_h = _;
                return population;
            }
            
            population.titles = function(_) {
                if(!arguments.length) return titles;
                titles = _;
                return population;
            }
                    
               return population;  
            };
            
                 
            function populationId(d) {
                return d.id;
            }
                    
            function populationStatistic(d) {
                return d.statistic;
            }
        
    })();