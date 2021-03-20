
 (el, x) => {

  let timex = [];

  d3.selectAll('.node').each(function(d,i) {
    let str = d3.select(this).attr("transform");
    str = str.match("([0-9]*\\.?[0-9]*),")[1];
    str = parseFloat(str);
    timex.push(str);
  });

  timex = [...new Set(timex)];
  let timepoints = d3.select('svg')
      .select('g')

  let time_labels = 0.0001;

  if (time_labels !== 0.0001 && time_labels.length === timex.length){
    for (let x = 0; x < timex.length; x++){
      timepoints
        .append('text')
        .attr('transform', 'translate('+(timex[x]-5)+',-5)')
        .attr('font-size', '1vw')
        .text(time_labels[x])
    }
  }
  

  

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
    
  d3.select('svg')
    .call(tip1);
    
  d3.select('svg')
    .call(tip2);

  
  let link = d3.selectAll('.link');
  link.style('stroke-opacity', 0.901);
  
  let linkText = d3.select('svg').append('g');
  let data = d3.selectAll('.link').data();
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



  d3.selectAll('.link')
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
  
  d3.selectAll('.node')
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
 