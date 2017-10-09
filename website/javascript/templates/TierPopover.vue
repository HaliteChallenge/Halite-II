<template>
  <el-popover
    placement="top"
    width="320"
    trigger="click"
    v-model="show"
    popper-class="tier-popover"
  >
    <span slot="reference" class="tier-clickable" :class="tier"></span>
    <div class="tier-description">
      <div class="header">
        <p class="header-text">{{badge}} tier</p>
        <span class="close-btn icon-remove" @click="show = false" />
      </div>
      <p class="tier-won">Won 145 games</p>
      <p class="tier-percetage"><i>5% of players achieved this badge</i></p>
      <div class="tier-more-btn">
        <button class="btn">
          <span>LEARN MORE ABOUT TIERS</span>
        </button>
      </div>
    </div>
  </el-popover>
</template>

<script>
  import Vue from 'vue';
  import {Popover} from 'element-ui';
  const badges = ['Diamond', 'Platinum', 'Gold', 'Silver', 'Salt'];

  export default {
    name: 'TierPopover',
    props: ['tier'],
    components: {
      'el-popover': Popover
    },
    mounted: function() {
      const index = parseInt(this.tier.split('-')[2]);
      const badge = badges[index - 1];
      this.index = index;
      this.badge = badge;
    },
    data: function() {
      return {
        show: false,
        index: '',
        badge: ''
      }
    }
  }
</script>

<style lang="scss">
  .tier-clickable {
    cursor: pointer;
  }
  .tier-popover {
    background-color: rgb(36, 37, 44);
    border-color: rgb(36, 37, 44);
    padding: 10px 15px 15px;

    .popper__arrow {
      border-top-color: rgb(36, 37, 44) !important;
      border-bottom-color: rgb(36, 37, 44) !important;
      &::after {
        border-top-color: rgb(36, 37, 44) !important; 
        border-bottom-color: rgb(36, 37, 44) !important; 
      }
    }
  }

  .tier-description {

    color: #FFFFFF;
    text-align: left;

    p {
      margin: 5px 0;
      font-size: 14px;
    }

    .header {
      display: flex;
      justify-content: space-between;
      align-items: center;
    }

    .header-text {
      margin: 0;
      text-transform: Uppercase;
      color: rgb(99, 205, 202);
      font-size: 24px;
      letter-spacing: 1px;
      font-family: Teko !important;
    }

    .close-btn {
      cursor: pointer;
    }

    .tier-more-btn {
      margin-top: 10px;
    }
  }
</style>