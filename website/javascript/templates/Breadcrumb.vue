<template>
  <div>
    <p>
      <span v-for="(anchor, index) in path">
        <a v-if="anchor.name || index !== path.length - 1" :href="anchor.link" class="c-ylw mg-right">{{anchor.name}}</a>
        <span v-else-if="index === path.length - 1 && !anchor.link" class="c-ylw mg-right">{{anchor.name}}</span>
        <img class="mg-right" v-if="index !== path.length - 1" :src="`${baseUrl}/assets/images/tail-triangle-right.svg`"/>
      </span>
    </p>
  </div>
</template>

<script>
  export default {
    name: 'HaliteBreadcrumb',
    props: {
      path: {
        type: Array,
        required: true
      },
      baseUrl: {
        type: String,
        required: true
      }
    }
  }
</script>

<style lang="scss" scoped>

  .c-ylw {
    color: #D19E07;
  }

  .mg-right {
    margin-right: 10px;
  }
</style>