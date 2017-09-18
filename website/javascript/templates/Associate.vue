<template>
    <div class="associate-container">

        <div class="row">
            <div class="col-md-2">
                <ul class="list-ha">
                    <li class="active">
                        <i class="xline xline-top"></i>
                        <i class="xline xline-bottom"></i>
                        <a href="">Personal Info</a>
                    </li>
                    <li>
                        <i class="xline xline-top"></i>
                        <i class="xline xline-bottom"></i>
                        <a href="">Account Info</a>
                    </li>
                </ul>
            </div>
            <div class="col-md-8 col-xm-10">
                <div class="page-header">
                    <h1>Create New Account</h1>
                    <p>Additional information are required to create your account.</p>
                </div>
                <h2 class="form-heading">personal info</h2>
                <form v-on:submit.prevent="submit" class="create-account-form">
                    <!-- profession -->
                    <div class="form-group">
                        <label for="country">Which of the following describes you best?<span class="text-danger">*</span></label>
                        <select class="form-control" id="level" v-model="level">
                            <option>Professional</option>
                            <option value="Undergraduate">In university (undergraduate)</option>
                            <option value="Graduate">In graduate school</option>
                            <option value="High School">In high school</option>
                        </select>
                    </div>

                    <div v-if="level === 'Professional'">
                        <div class="form-group">
                            <label for="organization">Please choose your organization<span class="text-danger">*</span></label>
                            <select class="form-control" id="organization" v-model="organization">
                                <option value="NONE">(no affiliation)</option>
                                <option v-for="org in organizations" :value="org.organization_id">{{ org.name }} ({{org.type}})</option>
                            </select>
                        </div>

                        <div class="form-group" v-if="organization != 'NONE'">
                            <label for="work-email">Please share your work email</label>
                            <p class="help-block">We won’t share it publicly, plus you can see how you score against your coworkers</p>
                            <input type="email" class="form-control" id="work-email" placeholder="Work Email" aria-describedby="work-email-help" v-model="email" />
                        </div>

                        <div class="form-group">
                            <label for="org-name">Or, if you can't find your organization, share its name with us for approval</label>
                            <span id="org-name-help" class="help-block">This will be show on your profile and leaderboard</span>
                            <input type="text" class="form-control" id="org-name" placeholder="Name" aria-describedby="org-name-help" />
                        </div>
                    </div>

                    <div v-if="level === 'Undergraduate' || level === 'Graduate'">
                        <div class="form-group">
                            <label for="organization">Please choose your school<span class="text-danger">*</span></label>
                            <select class="form-control" id="organization" v-model="organization">
                                <option v-for="org in only_universities" :value="org.organization_id">{{ org.name }} ({{org.type}})</option>
                            </select>
                        </div>

                        <div class="form-group">
                            <label for="school-email">Please share your school email</label>
                            <p class="help-block">We won’t share it publicly, plus you can see how you score against your coworkers</p>
                            <input type="email" class="form-control" id="school-email" placeholder="School email" aria-describedby="school-email-help" v-model="email" />
                        </div>
                    </div>

                    <div v-if="level === 'High School'">
                        <div class="form-group">
                            <label for="organization">Please choose your school<span class="text-danger">*</span></label>
                            <select class="form-control" id="organization" v-model="organization">
                                <option v-for="org in only_high_schools" :value="org.organization_id">{{ org.name }} ({{org.type}})</option>
                            </select>
                        </div>

                        <div class="form-group">
                            <label v-if="organization != null" for="high-school-password">Great! Just enter your school password here:</label>
                            <input v-if="organization != null" type="text" class="form-control" id="high-school-password" placeholder="School password" aria-describedby="high-school-password-help" />
                            <span id="high-school-password-help" class="help-block">See your teacher if you don't know this</span>
                        </div>
                    </div>



                    <h2 class="form-heading">Account info</h2>

                    <!-- country -->
                    <div class="form-group">
                        <label for="country">Which country will you be playing from?</label>
                        <select class="form-control" id="country" aria-describedby="country-help" v-model="country_code">
                            <option value="NONE">(would prefer not to disclose)</option>
                            <option v-for="country in countries" :value="country[0]">{{ country[1].name }}</option>
                        </select>
                    </div>

                    <div class="form-group" v-if="country_code !== 'NONE'">
                        <label for="country">What is your state, province, or region?</label>
                        <select class="form-control" id="country-region" aria-describedby="country-region-help" v-model="country_region_code">
                            <option value="NONE">(would prefer not to disclose)</option>
                            <option v-for="region in regions" :value="region[0]">{{ region[1].name }}</option>
                        </select>
                    </div>

                    <div class="form-group has-error" v-if="error">
                        <span id="error-help" class="help-block">{{ error }}</span>
                    </div>

                    <button type="submit" class="btn-ha btn-ha-lg">Submit</button>
                </form>
            </div>
        </div>

    </div>
</template>

<script>
    import * as api from "../api";

    export default {
        name: "associate",
        data: function() {
            const countries = Object.entries(iso3166.data);
            countries.sort(function(country1, country2) {
                const country1name = country1[1].name;
                const country2name = country2[1].name;
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

            return {
                countries: countries,
                data: iso3166.data,
                email: "",
                country_code: "NONE",
                country_region_code: "NONE",
                level: "Professional",
                organization: null,
                organizations: [],
                error: null,
            };
        },
        computed: {
            regions: function() {
                const regions = Object.entries(iso3166.data[this.country_code].sub);
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
                return regions;
            },
            only_universities: function() {
                return this.organizations.filter((org) => org.type === "University");
            },
            only_high_schools: function() {
                return this.organizations.filter((org) => org.type === "High School");
            },
        },
        methods: {
            submit: function() {
                let request = {
                    "level": this.level,
                    "organization_id": this.organization === "NONE" ? null : this.organization,
                };

                if (this.country_code !== "NONE") {
                    const codes = {};

                    // Build conversion table of 2-char country code to 3-char
                    for (let code3 of Object.keys(iso3166.codes)) {
                        codes[iso3166.codes[code3]] = code3;
                    }

                    request["country_code"] = codes[this.country_code];
                    if (this.country_region_code !== "NONE") {
                        request["country_subdivision_code"] = this.country_region_code;
                    }
                }

                if (this.level !== "High School" && this.email) {
                    request["email"] = this.email;
                }

                console.log(request);
                api.register_me(request).then((success) => {
                    window.location.replace("/user?me");
                }, (error) => {
                    this.error = error.responseJSON.message;
                });
            },
        },
        mounted: function() {
            api.me().then((me) => {
               if (!me.is_new_user) {
                   window.location.replace("/user?me");
               }
            });
            api.list_organizations().then((orgs) => {
               this.organizations = orgs;
            });
        },
    }
</script>

<style lang="scss" scoped>
    .associate-container{
        padding-top: 50px;
    }
</style>
