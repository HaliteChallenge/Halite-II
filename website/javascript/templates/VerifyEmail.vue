<template>
    <form>
        <p class="text-danger" style="text-align: center; padding-left: 40px;padding-right: 40px;">{{ error_message }}</p>
        <p style="text-align: center;"> {{ success_message }} </p>
    </form>
</template>

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
                window.setTimeout(function (user_id) {
                  window.location.replace('/user/?user_id='+user_id)
                }, 10000, this.user_id)
              }
            })
          }
        }
      }
    }
</script>

<style lang="scss" scoped>

</style>
