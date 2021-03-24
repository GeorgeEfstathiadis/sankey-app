
 (el, x) => {
  d3.select('#SankeyPlot').style('height', '820px')

  let svg = d3.select('svg');

  svg.attr('viewBox', "0,0,1287.546875,940")
  
  d3.select('#legend_here').remove()
  svg.append('g').attr('id', 'legend_here');

  let link = d3.selectAll('.link');
  let node = d3.selectAll('.node');

  /* Timepoints on Graph */
  let timex = [];

  node.each(function(d,i) {
    let str = d3.select(this).attr("transform");
    str = str.match("([0-9]*\\.?[0-9]*),")[1];
    str = parseFloat(str);
    timex.push(str);
  });

  timex = [...new Set(timex)];
  let timepoints = svg
      .select('g')

  let time_labels = 0.0001;

  if (time_labels !== 0.0001 && time_labels.length === timex.length){
    for (let x = 0; x < timex.length; x++){
      timepoints
        .append('text')
        .attr('transform', 'translate('+(timex[x]-5)+',-5)')
        .attr('font-size', '1vw')
        .attr('font-weight', 'bold')
        .text(time_labels[x])
    }
  }

  /* Legend */

  let legend_bool = false;
  if (legend_bool){
    d3.select('#legend_here').remove()
    svg.append('g').attr('id', 'legend_here');
    

    let legend = d3.select('#legend_here');

    let unique_nodes = [];
    let unique_colors = [];
    node.each(function(d,i) {
      unique_colors.push(d3.select(this).select('rect').style('fill'));
      unique_nodes.push(d3.select(this).select('text').text());
    });

    unique_nodes = [...new Set(unique_nodes)];
    unique_colors = [...new Set(unique_colors)];
    let larg_width = 0;
    let cur_width = 0;
    let distance = 0;
    let y = 0;
    if (unique_colors.length === unique_nodes.length){
      for (let x = 0; x < unique_nodes.length; x++){
        if (Math.floor(x/4) === y + 1){
          larg_width = larg_width + 50;
          distance = distance + larg_width;
          larg_width = 0;
        }

        y = Math.floor(x/4);
        legend.append("circle")
          .attr("cx",60 + distance)
          .attr("cy",830 + 30*(x-4*y))
          .attr("r", 6)
          .style("fill", unique_colors[x])
        
        legend.append("text")
          .attr("x", 80 + distance)
          .attr("y", 830 + 30*(x-4*y))
          .text(unique_nodes[x])
          .style("font-size", "15px")
          .attr("alignment-baseline","middle")

        cur_width = legend
          .selectAll('text')
          .nodes()[x]
          .getBBox()
          .width;

        larg_width = Math.max(larg_width, cur_width);

      }
    }



    if (80+distance>window.innerWidth){
      d3.select('#legend_here').remove()
      svg.append('g').attr('id', 'legend_here');
      

      legend = d3.select('#legend_here');


      let larg_width = 0;
      let cur_width = 0;
      let distance = 0;
      let y = 0;
      if (unique_colors.length === unique_nodes.length){
        for (let x = 0; x < unique_nodes.length; x++){
          if (Math.floor(x/4) === y + 1){
            larg_width = larg_width + 50;
            distance = distance + larg_width;
            larg_width = 0;
          }

          y = Math.floor(x/4);
          legend.append("circle")
            .attr("cx",60 + distance)
            .attr("cy",830 + 30*(x-4*y))
            .attr("r", 6)
            .style("fill", unique_colors[x])

          
          legend.append("text")
            .attr("x", 80 + distance)
            .attr("y", 830 + 30*(x-4*y))
            .text((unique_nodes[x].length>10) ? unique_nodes[x].substring(0, 11) + '...' : unique_nodes[x])
            .style("font-size", "15px")
            .attr("alignment-baseline","middle")

          cur_width = legend
            .selectAll('text')
            .nodes()[x]
            .getBBox()
            .width;

          larg_width = Math.max(larg_width, cur_width);

        }
      }


    }

    node
      .select('text')
      .attr('opacity', 0);
  } else {
    node
      .select('text')
      .attr('opacity', 1);

    d3.select('#legend_here').remove()
    svg.append('g').attr('id', 'legend_here');
  }



  /* Tooltip */

  

  d3.selectAll('title').remove();
  
  let tip1 = d3.tip()
    .attr('class', 'd3-tip')
    .style('background', 'rgba(0, 0, 0, 0.8)')
    .style('padding', '6px')
    .style('color', '#fff')
    .style('border-radius', '4px')
    .style('opacity', 0)
    .style('pointer-events', 'none')
    .attr('class', 'noselect')
    .offset([-10, 0])
    .html(d => {
      return d.source.name + ' -> ' + d.target.name + '<br><strong>' + d.value + '</strong> people in this path,' + '<br>which started from ' + d.ORIGIN;
    });
    
  let tip2 = d3.tip()
    .attr('class', 'd3-tip')
    .style('background', 'rgba(0, 0, 0, 0.8)')
    .style('padding', '6px')
    .style('color', '#fff')
    .style('border-radius', '4px')
    .style('opacity', 0)
    .style('pointer-events', 'none')
    .attr('class', 'noselect')
    .offset([-10, 0])
    .html(d => {
      return d.name + '<br><strong>' + d.value + '</strong> people in this node!';
    });
    
  svg.call(tip1);
    
  svg.call(tip2);

  
  link.style('stroke-opacity', 0.901);
  

  /* Link Text button */
  let linkText = svg.append('g');
  let data = link.data();
  let linkLength = data.length;

  
  
  let linkShow = false;
  let clicks = 0;
  d3.select('label[for=\"link_show\"]')
    .on('click', d => {
      linkShow = !linkShow;
      clicks = clicks + 1;
      if (clicks == 1){
        for (let x = 0; x < linkLength; x++){
          let d = data[x];
          linkText
              .append('text')
              .attr('class', 'linkText')
              .attr('x', -50 + d.source.x + (d.target.x - d.source.x) / 2)
              .attr('y', 50 + d.source.y + d.sy + (d.target.y + d.ty - d.source.y - d.sy) / 2)
              .attr('dy', '.35em')
              .attr('text-anchor', 'end')
              .attr('transform', null)
              .text('Origin: ' + d.ORIGIN + '/ ' + d.source.name + ' -> ' + d.target.name + ': ' + d.value)
              .attr('font-weight', 'bold')
              .attr('text-anchor', 'start')
              .attr('opacity', 0);
        } 
      }
      if (linkShow){
        d3.selectAll('.linkText')
          .attr('opacity', 1)
      } else {
        d3.selectAll('.linkText')
          .attr('opacity', 0)
      }
    })

  /* Node Labels Hide */

  let nodeHide = false;
  d3.select('label[for=\"remove_labels\"]')
    .on('click', d => {
      nodeHide = !nodeHide;
      if (nodeHide){
        node
          .select('text')
          .style('opacity', 0);
      } else {
        node
          .select('text')
          .style('opacity', 1);
      }
    })



  link
    .on('mouseover', function(d){
      tip1.show(d)
        .style('pointer-events', 'none')
        .style('opacity', 0.9);

      
        

      //c
      
    })
    .on('mouseout',function(d){
      tip1.hide(d);
      
      //d
    })
    
  let fill;
  
  //a
  
  node
    .on('mouseover', function(d){
      tip2.style('opacity', 0.9)
        .show(d)
        .style('pointer-events', 'none');
      
      fill = d3.select(this)
                  .select('rect')
                  .style('fill');
                  
      d3.select(this)
                  .select('rect')
                  .style('fill', 'red');

      //e

    })
    .on('mouseout',function(d){
      tip2.hide(d);
      
      d3.select(this)
                  .select('rect')
                  .style('fill', fill);
                  
      //f
      
    })

    /* PowerBI click action */
    node
      .select('rect')
      .style("cursor", "pointer");
    node
      .on("mousedown.drag", null);

    let node_op;
    node
      .on("click", function(d,i){
        node_op = d3.select(this)
          .select('rect')
          .style('opacity');
        node_op = parseFloat(node_op);
        allnodes_op = Math.min(parseFloat(node.nodes()[0].firstChild.style.opacity), 
                              parseFloat(node.nodes()[1].firstChild.style.opacity));

        

        if ((node_op === 0.9 && allnodes_op === 0.9) || (node_op === 0.5)){
          node
            .select('rect')
            .style('opacity', '0.5')


          if (node.select('text').style('opacity') !== '0'){
            node
              .select('text')
              .style('opacity', '0.5')

            d3.select(this)
              .select('text')
              .style('opacity', '1')
          }


          d3.select(this)
            .select('rect')
            .style('opacity', '0.9')

          

          let i2 = 0;
          link.each(d2 => {
            if (d2.source === d || d2.target == d){
              link.nodes()[i2].style.strokeOpacity = '0.5';
              link.nodes()[i2].style.opacity = '';
            } else {
              link.nodes()[i2].style.opacity = '0.3';
            }
            i2 = i2+1;
          })


        } else if (node_op === 0.9 && allnodes_op === 0.5){
          node
            .select('rect')
            .style('opacity', '0.9')

          if (node.select('text').style('opacity') !== '0'){
            node
              .select('text')
              .style('opacity', '1')
          }

          

          link
            .style('opacity', '')

          link
            .style('stroke-opacity', '0.2')
        }
      })
    
    let today = new Date();
    let dd = String(today.getDate()).padStart(2, '0');
    let mm = String(today.getMonth() + 1).padStart(2, '0'); //January is 0!
    let yyyy = today.getFullYear();

    today = yyyy + '-' + mm + '-' + dd;

    d3.select('#downloadsvg').on('click', function() {
                d3.select(this)
                   .attr('href', 'data:application/octet-stream;base64,' + btoa(d3.select('#SankeyPlot').html()))
                   .attr('download', 'sankey-svg-network-' + today + '.svg')
             });

    
    
      

 }
 