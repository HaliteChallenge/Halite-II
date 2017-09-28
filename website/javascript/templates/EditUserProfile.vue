<template>
    <div class="associate-container edit-user-container">
        <div class="row">
            <div class="col-md-2">
                <a href="/user?me" class="back-arrow"><img class="arrow" :src="`${baseUrl}/assets/images/temp/back_arrow.png`"/><span>Back to your profile</span></a>
                <ul class="list-ha">
                    <li>
                        <i class="xline xline-top"></i>
                        <a href="#section_personal_info">Personal Info</a>
                    </li>
                    <li>
                        <i class="xline xline-top"></i>
                        <a href="#section_account_info">Account Info</a>
                    </li>
                </ul>
            </div>
            <div class="col-md-8 col-xm-10">
                <div class="page-header">
                    <a id="section_personal_info"></a>
                    <h1>Edit your profile</h1>
                </div>
                <h2 class="form-heading">personal info</h2>
                <form v-on:submit.prevent="submit" class="create-account-form">
                    <div class="form-group">
                        <label for="country">Which of the following describes you best?<span class="text-danger">*</span></label>
                        <select class="form-control" id="level" v-model="level">
                            <option>Professional</option>
                            <option value="University">University</option>
                            <option value="High School">High school</option>
                        </select>
                    </div>

                    <div v-if="level === 'Professional'">
                        <div class="form-group">
                            <label for="work-email">Please share your work email</label>
                            <p class="help-block">We won’t share it publicly, plus you can see how you score against your coworkers</p>
                            <input type="email" class="form-control" id="work-email" placeholder="Work Email" aria-describedby="work-email-help" v-model="email" />
                        </div>
                    </div>

                    <div v-if="level === 'University'">
                        <div class="form-group">
                            <label for="school-email">Please share your school email</label>
                            <p class="help-block">We won’t share it publicly, plus you can see how you score against your coworkers</p>
                            <input type="email" class="form-control" id="school-email" placeholder="School Email" aria-describedby="school-email-help" v-model="email" />
                        </div>
                    </div>

                    <div v-if="level === 'High School'">
                        <div class="form-group">
                            <label for="organization">Please enter your school</label>
                            <input type="text" class="form-control" placeholder="School Name" v-model="organization">
                        </div>
                    </div>
                    <div class="form-group">
                        <label for="organization">If you are playing as part of a Hackathon, please enter your code here</label>
                        <input type="text" class="form-control" v-model="hackathon_code" placeholder="Enter Hackathon password or code...">
                    </div>

                    <div class="line-container"><i class="xline xline-top"></i></div>

                    <h2 id="section_account_info" class="form-heading">Account info</h2>
                    <div class="form-group">
                        <label for="country">Username(as show On Github)</label>
                        <div class="relative-container">
                            <input type="text" class="form-control" placeholder="Julskast" disabled>
                            <i class="fa fa-lock lock"></i>
                        </div>
                    </div>
                    <div class="form-group">
                        <label for="country">Which country will you be playing from?<span class="text-danger">*</span></label>
                        <v-select
                            placeholder="(would prefer not to disclose)"
                            label="label"
                            v-model="country_code"
                            :options="country_options">
                        </v-select>
                    </div>

                    <div class="form-group" v-if="country_code !== ''">
                        <label for="country">State/province<span class="text-danger">*</span></label>
                        <v-select
                            placeholder="(would prefer not to disclose)"
                            v-model="country_region_code"
                            label="label"
                            :options="regions">
                        </v-select>
                    </div>

                    <div class="form-group has-error" v-if="error">
                        <span id="error-help" class="help-block">{{ error }}</span>
                    </div>
                    <a class="cancel-href base" href="#" target="_self">Cancel</a>
                    <div class="ha-button-container">
                        <div>
                            <a href="#" class="ha-button"><span>Update Profile</span></a>
                        </div>
                    </div>
                </form>
            </div>
        </div>

    </div>
</template>

<script>
    import * as api from "../api";
    import vSelect from 'vue-select';
    import {Alert, countries_data} from "../utils";

    export default {
        name: "EditUserProfile",
        components: {vSelect},
        props: ['baseUrl'],
        data: function() {
            const countries = Object.entries(iso3166.data);
            countries.sort(function(country1, country2) {
                const country1name = country1[1].name;
                const country2name = country2[1].name;
                const country1code = country1[0];
                const country2code = country2[0];
                if (country1code == 'US' && country2code != 'US') return -1;
                if (country2code == 'US' && country1code != 'US') return 1;

                if (country1name < country2name) {
                    return -1;
                }
                else if (country1name === country2name) {
                    return 0;
                }
                else {
                    return 1;
                }
            });

            const codes = {};
            Object.entries(iso3166.codes).forEach((item) => {
                codes[item[1]] = item[0];
            });
            const new_countries = countries.map((item) => {
                return {
                    label: item[1].name,
                    value: codes[item[0]],
                    code: item[0]
                };
            });
            new_countries.unshift({
                value: "NONE",
                code: "NONE",
                label: "(would prefer not to disclose)",
            });

            return {
                countries: countries,
                country_options: new_countries,
                data: iso3166.data,
                email: "",
                country_code: "",
                country_region_code: "",
                level: "Professional",
                organization: null,
                organizations: [],
                error: null,
                hackathon_code: '',
                primary:true,
            };
        },
        computed: {
            regions: function() {
                const regions = Object.entries(iso3166.data[this.country_code.code].sub);

                const codes = [];

                regions.sort(function(region1, region2) {
                    const name1 = region1[1].name;
                    const name2 = region2[1].name;
                    if (name1 < name2) {
                        return -1;
                    }
                    else if (name1 === name2) {
                        return 0;
                    }
                    else {
                        return 1;
                    }
                });

                const new_regions = regions.map((item) => {
                    return {
                        label: item[1].name,
                        value: item[0],
                        type: item[1].type
                    }
                });

                new_regions.unshift({
                    value: "NONE",
                    code: "NONE",
                    label: "(would prefer not to disclose)",
                });

                return new_regions;
            },
        },
        methods: {
            submit: function() {
                let request = {
                    "level": this.level,
                    "organization_id": this.organization === "NONE" ? null : this.organization,
                };

                if (this.country_code !== "") {
                    const codes = {};

                    // Build conversion table of 2-char country code to 3-char
                    for (let code3 of Object.keys(iso3166.codes)) {
                        codes[iso3166.codes[code3]] = code3;
                    }

                    request["country_code"] = this.country_code.value;
                    if (this.country_region_code !== "") {
                        request["country_subdivision_code"] = this.country_region_code.value;
                    }
                }

                if (this.level !== "High School" && this.email) {
                    request["email"] = this.email;
                }

                // verify email
                if (request["organization_id"] && (this.email === '')){
                    this.error = "Email is required";
                    return false;
                }
            },
        },
        mounted: function() {
        },
    }
</script>

<style lang="scss" scoped>
</style>
