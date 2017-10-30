<template>
    <div class="associate-container">
        <div class="row">
            <div class="col-md-6 col-xm-10 col-md-offset-3 col-xm-offset-1">
                <div class="page-header">
                    <a id="section_personal_info"></a>
                    <h1>Create New Account</h1>
                    <p class="text-center">Additional information is required to complete your account.</p>
                    <i class="xline xline-bottom"></i>
                </div>
                <h2 class="form-heading">personal info</h2>
                <form v-on:submit.prevent="submit" class="create-account-form">
                    <!-- profession -->
                    <div class="form-group">
                        <label for="country">Which of the following describes you best?<span class="text-danger">*</span></label>
                        <!-- <select class="form-control" id="level" v-model="level">
                            <option>Professional</option>
                            <option value="University">University</option>
                            <option value="High School">High school</option>
                        </select> -->
                        <v-select
                          placeholder="Professional"
                          v-model="level"
                          :options="['Professional', 'University', 'High School']">
                        </v-select>
                    </div>

                    <div v-if="level === 'Professional'">
                        <div class="form-group">
                            <label for="work-email">Please share your work email</label>
                            <input type="email" class="form-control" id="work-email" placeholder="Work Email" aria-describedby="work-email-help" v-model="email" />
                            <p style="margin-top: 10px;">We’ll use your email domain to affiliate you with your school or company, but we won’t share your email publicly.</p>
                        </div>
                    </div>

                    <div v-if="level === 'University'">
                        <div class="form-group">
                            <label for="school-email">Please share your school email</label>
                            <input type="email" class="form-control" id="school-email" placeholder="School Email" aria-describedby="school-email-help" v-model="email" />
                            <p style="margin-top: 10px;">We’ll use your email domain to affiliate you with your school or company, but we won’t share your email publicly.</p>
                        </div>
                    </div>

                    <div v-if="level === 'High School'">
                        <div class="form-group">
                            <label for="organization">Please select your school</label>
                            <v-select
                              placeholder="Select School"
                              v-model="selected_highSchool"
                              :options="highSchools">
                            </v-select>
                            <p style="margin-top: 10px;">If your school is not listed, please email us at <a href="mailto:halite@halite.io">halite@halite.io</a>.</p>
                        </div>
                    </div>

                    <h2 id="section_account_info" class="form-heading">Account info</h2>

                    <!-- country -->
                    <div class="form-group">
                        <label for="country">Which country will you be playing from?</label>
                        <v-select
                            placeholder="(would prefer not to disclose)"
                            label="label"
                            v-model="country_code"
                            :options="country_options">
                        </v-select>
                    </div>

                    <div class="form-group" v-if="country_code !== ''">
                        <label for="country">What is your state, province, or region?</label>
                        <v-select
                            placeholder="(would prefer not to disclose)"
                            v-model="country_region_code"
                            label="label"
                            :options="regions">
                        </v-select>
                    </div>

                    <!-- <h2 id="section_hackathons" class="form-heading">Hackathons</h2>
                    <div class="form-group">
                        <label for="organization">If you are playing as part of a Hackathon, please enter your code here</label>
                        <input type="text" class="form-control" v-model="hackathon_code" placeholder="Enter Hackathon password or code...">
                    </div> -->
                    <div class="form-group has-error" v-if="error">
                        <span id="error-help" class="help-block">{{ error }}</span>
                    </div>
                    <button type="submit" class="btn-ha btn-ha-md">Submit</button>
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
      name: 'associate',
      components: {vSelect},
      data: function () {
        const countries = Object.entries(iso3166.data)
        countries.sort(function (country1, country2) {
          const country1name = country1[1].name
          const country2name = country2[1].name
          const country1code = country1[0]
          const country2code = country2[0]
          if (country1code == 'US' && country2code != 'US') return -1
          if (country2code == 'US' && country1code != 'US') return 1

          if (country1name < country2name) {
            return -1
          } else if (country1name === country2name) {
            return 0
          } else {
            return 1
          }
        })

        const codes = {}
        Object.entries(iso3166.codes).forEach((item) => {
          codes[item[1]] = item[0]
        })

        const new_countries = countries.map((item) => {
          return {
            label: item[1].name,
            value: codes[item[0]],
            code: item[0]
          }
    })
        new_countries.unshift({
          value: 'NONE',
          code: 'NONE',
          label: '(would prefer not to disclose)'
        })

        return {
          countries: countries,
          country_options: new_countries,
          data: iso3166.data,
          email: '',
          country_code: '',
          country_region_code: '',
          level: 'Professional',
          organization: null,
          organizations: [],
          highSchools: [],
          selected_highSchool: null,
          error: null,
          hackathon_code: '',
          success_message: '',
          error_string: '',
          hackathon_error_message: ''
        }
  },
      computed: {
        regions: function () {
          const regions = Object.entries(iso3166.data[this.country_code.code].sub)

          const codes = []

          regions.sort(function (region1, region2) {
            const name1 = region1[1].name
            const name2 = region2[1].name
            if (name1 < name2) {
              return -1
            } else if (name1 === name2) {
              return 0
            } else {
              return 1
            }
          })

          const new_regions = regions.map((item) => {
            return {
              label: item[1].name,
              value: item[0],
              type: item[1].type
            }
          })

          new_regions.unshift({
            value: 'NONE',
            code: 'NONE',
            label: '(would prefer not to disclose)'
          })

          return new_regions
        }
      },
      methods: {
        submit: function () {
          let request = {
            'level': this.level,
            'organization_id': this.organization === 'NONE' ? null : this.organization
          }

          if (this.country_code !== '') {
            const codes = {}

            // Build conversion table of 2-char country code to 3-char
            for (let code3 of Object.keys(iso3166.codes)) {
              codes[iso3166.codes[code3]] = code3
            }

            request['country_code'] = this.country_code.value
            if (this.country_region_code !== '') {
              request['country_subdivision_code'] = this.country_region_code.value
            }
          }

          if (this.level !== 'High School' && this.email) {
            request['email'] = this.email
          }

          // verify email
          if (request['organization_id'] && (this.email === '')) {
            this.error = 'Email is required'
            return false
          }

          if(this.level === 'High School' && this.selected_highSchool){
          request['organization_id'] = this.selected_highSchool.id
          }

          //this.hackathon_error_message = ''
          api.register_me(request).then((response) => {
            let message = 'You have updated your profile successfully. You will be redirected automatically in a few seconds.';
            if (response.message)
              message += ' ' + response.message;
            Alert.show(message, 'success', true)
            this.gaData('account', 'new-account-success', 'account-flow')
            setTimeout(() => {
            window.location.replace('/learn-programming-challenge/?new=1')
            }, 3000)
          }, (error) => {
            const errorMessage = error.responseJSON
              ? error.responseJSON.message
              : "Sorry, we couldn't update your profile. Please try again later."
            Alert.show(errorMessage, 'error')
            this.gaData('account', 'new-account-error', 'account-flow')
          })
        },
        gaData: function (category, action, label) {
          utils.gaEvent(category, action, label)
        }
      },
      mounted: function () {
        api.me().then((me) => {
          if (me && !me.is_new_user) {
            window.location.replace('/user?me')
          } else {
            this.gaData('account', 'click-submit-new-account', 'account-flow')
          }
        })

      api.list_organizations().then((orgs)=>
      {
          let schools = []
          if(orgs && orgs instanceof Array)
          {
              schools = orgs.filter((item) => {
              return item.type === 'High School'
            })
          }

          for (var i = 0; i < schools.length; i++) {
              this.highSchools.push({label: schools[i].name, id: schools[i].organization_id})
            }
      })
    },
  }
</script>

<style lang="scss" scoped>
</style>
