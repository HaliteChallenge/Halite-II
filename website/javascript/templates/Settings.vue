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
                    <input v-if="api_key" style="color: black; max-width: 400px" type="text" class="form-control" readonly :value="api_key"/>
                    <button class="btn-ha btn-ha-md" style="margin-top:20px;" v-on:click="fetchApiKey">Generate API Key</button>
                </form>

                 <div class="line-container"><i class="xline xline-top"></i></div>

                <h2 class="form-heading">Bot Settings</h2>
                <form v-on:submit.prevent="submit" class="create-account-form">
                    <div class="form-group">
                      <label for="gpusettings">Enable GPUs for your current bot</label>
                       <v-select
                            label="label"
                            v-model="selected_gpu_state"
                            :options="gpu_states">
                        </v-select>
                    </div>


                    <div class="line-container"><i class="xline xline-top"></i></div>

                    <h2 class="form-heading">Email Settings</h2>
                    <form v-on:submit.prevent="submit" class="create-account-form">
                        <div class="form-group">
                                To manage your email preferences, click the relevant link at the bottom of any Halite email.
                        </div>
                    </form>

                    <a class="cancel-href base" href="/user/?me" target="_self">Cancel</a>
                    <button type="submit" class="btn-ha">Save Settings</button>
                </form>
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
          api_key: null,
        }
  },
      mounted: function () {

        this.gpu_states.push( {label: "Yes",value: 1});
        this.gpu_states.push( {label: "No",value: 0});

        api.me().then((me) => {
          this.user = me
          this.level = me.level
          this.username = me.username

          if(this.user.is_gpu_enabled){
            this.selected_gpu_state = this.gpu_states.find((item) => {
              return item.value == 1
            });
          }
          else{
              this.selected_gpu_state = this.gpu_states.find((item) => {
              return item.value == 0
            });
          }
        });
      },
      methods: {
        submit: function (e) {
           let request = {
            'is_gpu_enabled': this.selected_gpu_state.value,
          }

          api.update_me(this.user.user_id, request).then((response) => {
            let message = 'Your settings have been saved successfully.';
            if (response.message) message += ' ' + response.message;
            Alert.show(message, 'success', true)
            this.gaData('account', 'edit-settings-success', 'edit-settings-flow')
          }, (error) => {
            const errorMessage = error.responseJSON
              ? error.responseJSON.message
              : "Sorry, we couldn't update your settings. Please try again later."
            Alert.show(errorMessage, 'error')
            this.gaData('account', 'edit-settings-error', 'edit-settings-flow')
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
