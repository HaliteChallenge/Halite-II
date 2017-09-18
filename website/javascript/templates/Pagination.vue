<template>
  <div>
    <div class="page-container">
      <a href="javascript:;"><img :src="`${baseUrl}/assets/images/page-prev.svg`"/></a>
      <a href="javascript:;">First page</a>
      <a href="javascript:;" class="underline" v-for="i in pageRange">{{i}}</a>
      <span>...</span>
      <a href="javascript:;">Last page</a>
      <a href="javascript:;"><img :src="`${baseUrl}/assets/images/page-next.svg`"/></a>
    </div>
  </div>
</template>

<script>
  import _ from 'lodash';

  export default {
    name: 'HalitePagination',
    props: {
      page: {
        type: Number,
        required: true
      },
      lastPage: {
        type: Number,
        required: true
      },
      baseUrl: {
        type: String,
        required: true
      }
    },
    computed: {
      pageRange: function () {
        if(this.lastPage <= 6) {
          return _.range(1, this.lastPage + 1)
        } else {
          // TODO not finish
          return _.range(1, 7)
        }
      }
    }
  }
</script>

<style lang="scss" scoped>
  .page-container {
    text-align: center;
    span {
      margin-right: 5px;
    }
    a {
      margin-right: 10px;
    }
  }

  .underline {
    text-decoration: underline;
    cursor: pointer;
  }
</style>