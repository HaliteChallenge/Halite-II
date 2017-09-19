<template>
  <div>
    <div class="page-container">
      <a href="javascript:;" @click="updatePage(page - 1)"><img :src="`${baseUrl}/assets/images/page-prev.svg`"/></a>
      <a href="javascript:;" @click="updatePage(1)">First page</a>
      <a href="javascript:;" @click="updatePage(i)" :class="page === i? 'underline disabled': 'underline'" v-for="i in pageRange">{{i}}</a>
      <span v-show="hasMore">...</span>
      <a href="javascript:;" @click="updatePage(lastPage)">Last page</a>
      <a href="javascript:;" @click="updatePage(page + 1)"><img :src="`${baseUrl}/assets/images/page-next.svg`"/></a>
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
      },
      changePage: {
        type: Function,
        required: true
      }
    },
    data: function() {
      return {
        hasMore: false
      }
    },
    computed: {
      pageRange: function () {
        if(this.lastPage <= 6) {
          this.hasMore = false;
          return _.range(1, this.lastPage + 1);
        } else {
          if(this.page <= 3) {
            this.hasMore = true;
            return _.range(1, 7);
          } else if(this.page >= this.lastPage - 3) {
            this.hasMore = false;
            return _.range(this.lastPage - 5, this.lastPage + 1);
          } else {
            this.hasMore = true;
            return _.range(this.page - 2, this.page + 4);
          }
        }
      }
    },
    methods: {
      updatePage: function(page) {
        if(page > this.lastPage || page === this.page) return;
        this.changePage(page);
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
      color: inherit;

      img {
        height: 18px;
      }
    }
  }

  .underline {
    text-decoration: underline;
  }
  .disabled {
    text-decoration: none;
    cursor: text;
  }
</style>