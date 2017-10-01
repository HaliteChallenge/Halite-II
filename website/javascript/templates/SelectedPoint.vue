<template>
    <div class="map-props">
        <div class="map-props-icon">
            <span class="icon-planet"></span>
        </div>
        <div class="map-props-list">
            <table>
                <tr>
                    <td>Location:</td>
                    <td>{{info.location}}</td>
                </tr>
            </table>
        </div>
    </div>
</template>
<script>
 import Vue from "vue";

 export default {
     props: ['selectedPoint'],
     computed: {
         info: function(){
             const info = {
                 location: `${this.selectedPoint.x.toFixed(4)}, ${this.selectedPoint.y.toFixed(4)}`,
             };

             return info;
         }
     }
 }
</script>
