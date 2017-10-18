<template>
    <div class="settings-container associate-container edit-user-container">
        <div>
            <a href="/user?me" class="back-arrow"><img class="arrow" :src="`${baseUrl}/assets/images/temp/back_arrow.png`"/><span>Back to your profile</span></a>
        </div>
        <div class="row">
            <div class="col-md-6 col-xm-10 col-md-offset-3 col-xm-offset-1">
                <div class="page-header">
                    <a id="section_personal_info"></a>
                    <h1>Settings</h1>
                    <i class="xline xline-bottom"></i>
                </div>

                <h2 class="form-heading">API Key</h2>
                <p>
                     Your API key is used in Halite Client tools for authentication.
                </p>
                <br/>
                <p>
                     Note: Once you close this page, you will not be able to retrieve that key again. You will need to generate a new one.
                </p>
                <form>
                    <input style="color: black; max-width: 400px" type="text" class="form-control" readonly :value="api_key"/>
                    <button class="btn-ha btn-ha-md" style="margin-top:20px;" v-on:click="fetchApiKey">Generate API Key</button>
                </form>

                <h2 class="form-heading">Bot Settings</h2>
                <form v-on:submit.prevent="submit" class="create-account-form">
                    <div class="form-group">
                        <label for="hackathon">Enable GPUs for you bot</label>
                       <v-select
                            label="label"
                            v-model="selected_gpu_state"
                            :options="gpu_states">
                        </v-select>
                    </div>
                </form>
                <a class="cancel-href base" href="/user/?me" target="_self">Cancel</a>
                <button type="submit" class="btn-ha">Save Settings</button>
            </div>
        </div>
    </div>
</template>

<script>
import * as api from '../api'
import vSelect from 'vue-select'
import {Alert, countries_data} from '../utils'
import * as utils from '../utils'

export default {
      name: 'Settings',
      components: {vSelect},
      props: ['baseUrl'],
      data: function () {
        return {
          username: '',
          selected_gpu_state: null,
          gpu_states: [],
          error: null,
          user: null,
          api_key: ''
        }
  },
      mounted: function () {

        this.gpu_states.push("Yes");
        this.gpu_states.push("No");

        api.me().then((me) => {
          this.user = me
          this.level = me.level
          this.username = me.username

          if(this.user.is_gpu_enabled){
            this.selected_gpu_state = this.gpu_states.find((item) => {
              return item.value == "Yes"
            });
          }
          else{
             this.selected_gpu_state = this.gpu_states.find((item) => {
              return item.value == "No"
            });
          }
        });
      },
      methods: {
        submit: function (e) {
          api.update_me(this.user.user_id, request).then((response) => {
            let message = 'You have updated your profile successfully.';
            if (response.message) message += ' ' + response.message;
            Alert.show(message, 'success', true)
            this.gaData('account', 'edit-profile-success', 'edit-profile-flow')
          }, (error) => {
            const errorMessage = error.responseJSON
              ? error.responseJSON.message
              : "Sorry, we couldn't update your profile. Please try again later."
            Alert.show(errorMessage, 'error')
            this.gaData('account', 'edit-profile-error', 'edit-profile-flow')
          }).then(() => {
            if (this.hackathon_code) {
              api.registerHackathon(this.hackathon_code).then((response) => {
                let message = "You've signed up for the hackathon!"
                if (response.responseJSON && response.responseJSON.message) {
                  message = response.responseJSON.message
                }
                Alert.show(message, 'success', true)
              }, (err) => {
                let message = "Sorry, we couldn't sign you up for the hackathon. Please try again later."
                if (err.message) {
                  message = err.message
                }
                if (err.responseJSON) {
                  message = err.responseJSON.message
                }

                Alert.show(message, 'error')
              })
              this.hackathon_code = null
            }
          })
        },
        gaData: function (category, action, label) {
          utils.gaEvent(category, action, label)
        },
        fetchApiKey: function (e) {
          e.preventDefault()
          api.reset_api_key().then((data) => {
            this.api_key = data.api_key
            console.log(this.api_key)
            this.$forceUpdate()
          }, (e) => {
            console.error(e)
            this.api_key = 'Error: could not reset API key.'
          })
        },
      }
    }
</script>

<style lang="scss" scoped>
</style>
