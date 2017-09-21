<template>
  <div class="player-line-chart row">
    <svg width='100%' height='300' ref="mainSvg"></svg>
  </div>
</template>
<script>
  import Vue from "vue";
  import * as d3 from "d3";

  function getDataPeriod(_data, _length, _index) {
    if (!_data || !_length || (_data.length <= _length)) return _data
    let _output = []
    _data.forEach((item, i) => {
      let min 
      let max
      const maxIndex = _data.length - 1
      if((_index - _length / 2) < 0) {
        min = 0
        max = _length
      } else if ((_index + _length / 2) > maxIndex) {
        max = maxIndex
        min = maxIndex - _length
      } else {
        max = _index + _length / 2
        min = _index - _length / 2
      }
      if ((i >= min) && (i <= max)) {
        _output.push(item)
      }
    })
    return _output
  }

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
      maxLength: {
        type: Number,
        required: false,
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
        innerSvg = g1.append('foreignObject').attr('height', height).attr('width', width).append('svg');

        this.path1List = []
        this.path2List = []

        this.innerSvg = innerSvg

        var x = d3.scaleLinear()
          .rangeRound([0, width])

        this.x = x  

        var y = d3.scaleLinear()
          .rangeRound([height, 0])
        this.y = y

        var area = d3.area()
          .x(function(d) { return x(d.x) })
          .y1(function(d) { return y(d.y) })

        var line = d3.area()
            .x(function(d) { return x(d.x) })
            .y(function(d) { return y(d.y) })    

        this.area = area    
        this.line = line    



        // let color = d3.scaleOrdinal(d3.schemeCategory20c)

        // let color = d3.scaleLinear().domain([1, dataSet.length])
        //     .interpolate(d3.interpolateHcl)
        //     .range([d3.rgb("red"), d3.rgb('#f5bc13')])

        let dataSet = chartData

        let color = (index) => ['#E37222', '#BD00DB', '#63CECA', '#FFBE00', '#ECFFFB'][index] || '#ECFFFB'

        x.domain(d3.extent(getDataPeriod(dataSet[0], this.maxLength, this.index), function(d) { return d.x }))
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
          let path1 = g2.append('path')
          path1.datum(getDataPeriod(data, this.maxLength, this.index))
            .attr('stroke', _color)
            .attr('fill', 'transparent')
            .attr('stroke-linejoin', 'round')
            .attr('stroke-dasharray', '1 10')
            .attr('stroke-dashunits', 'pathLength')
            .attr('stroke-linecap', 'round')
            .attr('stroke-width', 2)
            .attr('d', line)

          this.path1List.push(path1)
            
          let path2 = innerSvg.append('path')
          path2.datum(getDataPeriod(data, this.maxLength, this.index))
            .attr('fill', _color)
            .attr('fill-opacity', .1)
            .attr('stroke', _color)
            .attr('stroke-linejoin', 'round')
            .attr('stroke-linecap', 'round')
            .attr('stroke-width', 3)
            .attr('d', area)
          
          this.path2List.push(path2)
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
      if (!this.path1List || !this.path2List || !this.path1List.length || !this.path2List.length || !this.innerSvg || !this.dragLine) return;

      let dataSet = this.chartData
      this.maxLength && console.log(index, getDataPeriod(dataSet[0], this.maxLength, index))
      this.x.domain(d3.extent(getDataPeriod(dataSet[0], this.maxLength, index), function(d) { return d.x }))

      dataSet.forEach((data, index) => {
        data.forEach(d => {
          d.x = d.x
          d.y = Number(d.y)
        })
        let _peroidData = getDataPeriod(data, this.maxLength, this.index)
        this.path1List[index] && this.path1List[index].datum(_peroidData).attr('d', this.line)
        this.path2List[index] && this.path2List[index].datum(_peroidData).attr('d', this.area)
        
      })

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