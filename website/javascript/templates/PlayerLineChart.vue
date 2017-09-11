<template>
  <div class="player-line-chart row">
    <svg width='100%' height='300' ref="mainSvg"></svg>
  </div>
</template>
<script>
  import Vue from "vue";
  import * as d3 from "d3";

  export default{
    name: 'PlayerLineChart',

    props: {
      chartData: {
        type: Array,
        required: true,
      },
      index: {
        type: Number,
        required: true,
      },

    },

    data: function() {
      return {
        innerSvg: null,
        dragLine: null,
        x: null,
      }
    },

    watch: {
      chartData: function(chartData) {
        if(!chartData || !chartData.length) return

        var svg = d3.select(this.$refs.mainSvg).attr('class', 'main-svg'),
        svgPosition = svg.node().getBoundingClientRect(),
        margin = {top: 20, right: 20, bottom: 30, left: 50},
        width = +svgPosition.width - margin.left - margin.right,
        height = +svgPosition.height - margin.top - margin.bottom,
        // g1 = svg.append('foreignObject').attr('transform', 'translate(' + margin.left + ',' + margin.top + ')').append('svg'),
        g1 = svg.append('g').attr('transform', 'translate(' + margin.left + ',' + margin.top + ')'),
        g2 = svg.append('g').attr('transform', 'translate(' + margin.left + ',' + margin.top + ')'),
        innerSvg = g1.append('foreignObject').append('svg');

        this.innerSvg = innerSvg

        var x = d3.scaleLinear()
          .rangeRound([0, width])

        this.x = x  

        var y = d3.scaleLinear()
          .rangeRound([height, 0])

        var area = d3.area()
          .x(function(d) { return x(d.x) })
          .y1(function(d) { return y(d.y) })

        var line = d3.area()
            .x(function(d) { return x(d.x) })
            .y(function(d) { return y(d.y) })    


        let dataSet = chartData

        // let color = d3.scaleOrdinal(d3.schemeCategory20c)

        // let color = d3.scaleLinear().domain([1, dataSet.length])
        //     .interpolate(d3.interpolateHcl)
        //     .range([d3.rgb("red"), d3.rgb('#f5bc13')])

        let color = (index) => ['#ff3bfb', '#f5c022', '#94e6e3', '#e78744'][index] || '#e78744'

        x.domain(d3.extent(dataSet[0], function(d) { return d.x }))
        let _dataSet = []
        dataSet.forEach(_data => {
          _dataSet = _dataSet.concat(_data)
        })
        y.domain([0, d3.max(_dataSet, function(d) { return d.y })])

        area.y0(y(0))


        let yAxis = g1.append('g')
          .call(d3.axisLeft(y).ticks(5, 's'))
            yAxis.selectAll('.domain').attr('stroke', 'white')
            yAxis.selectAll('line').attr('stroke', '#a7b5b5').attr('x2', width).attr('opacity', .2)
            yAxis.selectAll('.tick text').attr('fill', '#a7b5b5').attr('font-size', '1.5rem')
            yAxis.selectAll('.domain')
            yAxis.select('.domain')
              .remove()

        innerSvg.attr('width', 0)
          .attr('height', height)

        dataSet.forEach((data, index) => {
          data.forEach(d => {
            d.x = d.x
            d.y = Number(d.y)
          })

        let _color = color(index)
        g2.append('path')
          .datum(data)
          .attr('stroke', _color)
          .attr('fill', 'transparent')
          .attr('stroke-linejoin', 'round')
          .attr('stroke-dasharray', '1 10')
          .attr('stroke-dashunits', 'pathLength')
          .attr('stroke-linecap', 'round')
          .attr('stroke-width', 2)
          .attr('d', line)
          
        innerSvg.append('path')
          .datum(data)
          .attr('fill', _color)
          .attr('fill-opacity', .1)
          .attr('stroke', _color)
          .attr('stroke-linejoin', 'round')
          .attr('stroke-linecap', 'round')
          .attr('stroke-width', 3)
          .attr('d', area)
      })


      let dragLine = g1.append('path')
        .attr('class', 'drag-line')
        .attr('d', `M${0},${y(0)}L${0},0`)
        .attr('stroke', '#a7b5b5')
        .attr('stroke-width', 3)

      this.dragLine = dragLine

      const self = this
        
      dragLine.call(
          d3.drag()
            .on('start', function(){ return d3.select(this).raise() })
            .on('drag', function(){
              let dx = d3.event.sourceEvent.clientX
              let _x = dx - margin.left - svgPosition.left
              let _base = 0
              let _width = width
              let _offset = _x < _base ? _base : _x > _width ? _width : _x
              self.$emit('updateIndex', Math.round(x.invert(_offset)))
              // innerSvg.attr('width', _offset)
              // d3.select(this)
              //   .attr('transform', () => {
              //     return `translate(${_offset})`
              //   })
            })
        )
    },
    index: function(index) {
      if (!this.innerSvg || !this.dragLine) return;
      this.innerSvg
        .transition()  
        .ease(d3.easeLinear)
        .attr('width', this.x(index))

      this.dragLine
        .transition()  
        .ease(d3.easeLinear)
        .attr('transform', `translate(${this.x(index)})`)
      },
    }
  }
</script>

<style lang="scss" scoped>
  .drag-line {
    cursor: col-resize;
  }
</style>