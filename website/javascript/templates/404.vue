<template>
  <div class="home-container view404-container">
      <div>
          <p class="d1 c-org font-headline" style="font-size: 30px;"> Are you looking for Halite Season 1?</p>
          <div class="ha-button-container no-bg-button">
            <div>
                <a class="ha-button" href="https://2016.halite.io/" target="_blank"><span>Halite Season 1</span></a>
            </div>
          </div>
          <div class="text-center" style="margin-top:15px;">
               <img :src="`/assets/images/temp/bot_1.png`" style="height:auto;width: 20%;"/>
          </div>
          <p class="d1 c-org font-headline" style="padding-top:15px;"> No? Nothing to see here ... move along... move along.</p>
      </div>
  </div>
</template>
Nothing to see here ... move along... move along
<script>
    import * as api from '../api'

export default {
      name: 'verify-email',
      data: function () {
        return {
          verification_code: '',
          user_id: '',
          error_message: '',
          success_message: ''
        }
  },
      mounted: function () {
        const params = new URLSearchParams(window.location.search)

        if (params.has('user_id') && params.has('verification_code')) {
          this.user_id = params.get('user_id')
          this.verification_code = params.get('verification_code')
          this.submit()
        }
      },
      methods: {
        submit: function () {
          if (this.verification_code && this.user_id) {
            $.post({
              url: `${api.API_SERVER_URL}/user/${this.user_id}/verify`,
              data: {
                verification_code: this.verification_code
              },
              error: (xhr) => {
                this.error_message = 'Your email verification failed, please contact halite@halite.io: Error Details -' + xhr.responseJSON.message
              },
              success: (xhr) => {
                this.success_message = 'Your email has been verified successfully. If you signed up for a hackathon that requires a verified email, you will now appear on the hackathon leaderboard. \n Sit tight, you’ll be automatically redirected in a few seconds.'
              }
            }).then(() => {
              window.setTimeout(function () {
                window.location.replace('/user/?me')
              }, 10000)
            })
          }
        }
      }
    }
</script>

<style lang="scss" scoped>

</style>
