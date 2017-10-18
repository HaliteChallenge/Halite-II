<template>
    <div class="row">
        <div class="col-md-6 col-xm-10 col-md-offset-3 col-xm-offset-1">
            <p>
                Your API key is used in Halite Client tools for authentication
            </p>
            <p>
                <strong>Note: </strong>Once you close this page, you will not be able to retrieve that key again. You will need to generate a new one.
            </p>
            <form>
                <input style="margin-top:20px; max-width: 400px" type="text" readonly :value="api_key"/>
                <button class="btn-ha btn-ha-md" style="margin-top:20px;" v-on:click="fetch">Generate API Key</button>
            </form>
        </div>
    </div>
</template>

<script>
    import * as api from '../api'

export default {
      name: 'ApiKey',
      data: function () {
        return {
          api_key: ''
        }
  },
      methods: {
        fetch: function (e) {
          e.preventDefault()
          api.reset_api_key().then((data) => {
            this.api_key = data.api_key
          }, (e) => {
            console.error(e)
            this.api_key = 'Error: could not reset API key.'
          })
        }
      }
    }
</script>

<style lang="scss" scoped>
    input {
        color: #000;
        width: 100%;
    }
</style>