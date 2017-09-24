<template>
  <div :class="`ha-alert ha-alert-${type}`" v-if="show">
    <button class="close" @click="disable">&times;</button>
    <span class="ha-alert-type">{{`${type.toUpperCase()}:`}}</span>{{` ${message}`}}
  </div>
</template>

<script>
  export default{
    name: "home",
    props: {
      "message": {
        type: String,
        default: "",
      },
      "type": {
        type: String,
        default: "success",
      }
    },
    data: function(){
      return {
        show: true
      }
    },
    methods: {
      disable: function(){
        console.log('hide message');
        this.show = false;
      }
    }
  }  
</script>
<style lang="scss" scoped>
  .ha-alert{
    padding: 20px 50px;
    text-align: center;
    font-size: 16px;
    line-height: 30px;
    .close{
      margin-right: 15px;
    }
    &-type{
      font-family: Teko;
      text-transform: uppercase;
      vertical-align: center;
      font-size: 24px;
      line-height: 30px;
    }

    &-success{
      background-color: #2ecc71;
      color: #262430;
      .close{
        color: #262430;
      }
    }
    &-error{
      background-color: #FF2D55;
      color: #fff;
      .close{
        color: #fff;
      }
    }
    button.close{
      opacity: 0.8;
      &:hover{
        opacity: 1;
      }
    }
  }
</style>

