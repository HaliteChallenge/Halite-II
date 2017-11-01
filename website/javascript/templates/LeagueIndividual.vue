<template>
  <div class="league-individual-container">
    <!-- breakcrumb -->
    <div class="page-header">
      <h1>HALITE LEAGUES</h1>
      <i class="xline xline-bottom"></i>
    </div>
    <div class="league-description">
      <p>league décription goes here</p>

      <div class="popup-overlay" v-show="sharePopup" @click="toggleShare"></div>
        <div class="popup-container" v-show="sharePopup">
          <div class="popup-share">
            <label>Share as a link</label>
            <div class="form-inline-button">
              <input ref="shareInput" type="text" :value="shareLink"> 
              <button class="btn" @click="copyToClipboard">
                <span>Copy</span>
              </button>
            </div>
            <div class="share-socials">
              <a :href="shareSocial('facebook')" onclick="javascript:window.open(this.href, '', 'menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=300,width=600');return false;"
target="_blank"><i class="fa fa-facebook-official"></i></a>
              <a :href="shareSocial('twitter')" onclick="javascript:window.open(this.href, '', 'menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=300,width=600');return false;"
target="_blank"><i class="fa fa-twitter"></i></a>
              <a :href="shareSocial('linkedin')" onclick="javascript:window.open(this.href, '', 'menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=300,width=600');return false;"
target="_blank"><i class="fa fa-linkedin"></i></a>
            </div>
            <!-- <div class="hr"></div>
            <label>Share as a video</label>
            <a href="#" class="btn btn-block"><span>Create Video</span></a> -->
          </div>
        </div>
        <button class="btn-ha" @click="toggleShare">
          <span>SHARE</span>
        </button>
    </div>

    <div class="table-container">
      <table class="table table-leader leagues-table">
        <thead>
          <tr>
            <th class="league-col-name">League name</th>
            <th class="league-col-desc">Description</th>
            <th class="league-col-category">Category</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td>
              <a :href="`/league?id=`">xzx</a>
            </td>
            <td>abc</td>
            <td>sss</td>
          </tr>
        </tbody>
      </table>
    </div>
  </div>
</template>

<script>
  export default {
    name: "LeagueIndividual",
    props: ['baseUrl'],
    data: function(){
      return {
        sharePopup: false,
        shareLink: '',
      }
    },
    mounted: function(){

    },
    methods: {
      toggleShare: function(){
        this.sharePopup = !this.sharePopup;
      },
      copyToClipboard: function(){
        if (e) e.preventDefault()
        this.$refs.shareInput.select()
        document.execCommand('copy')
      },
      shareSocial: function (social) {
        let text = 'Halite Game Replay - '
        let tags = 'halitegame'
        switch (social) {
          case 'facebook':
            return 'https://www.facebook.com/sharer.php?u=' + encodeURIComponent(window.location.href)
            break
          case 'twitter':
            return 'https://twitter.com/intent/tweet?text=' + text + '&url=' + encodeURIComponent(window.location.href) + '&hashtags=' + tags + '&via=haliteAI'
            break
          case 'linkedin':
            return `https://www.linkedin.com/shareArticle?mini=true&url=${encodeURIComponent(window.location.href)}`
            break
        }
      },
    }
  }
</script>