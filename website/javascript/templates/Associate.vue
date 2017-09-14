<template>
    <div class="associate-container">

        <div class="row">
            <div class="col-md-2">
                <ul>
                    <li><a href="">Personal Info</a></li>
                    <li><a href="">Account Info</a></li>
                </ul>
            </div>
            <div class="col-md-10">
                <div class="page-header">
                    <h1>Create New Account</h1>
                </div>
                <div class="page-description">
                    <p>
                        Additional information are required to create your account.
                    </p>
                </div>
                <form v-on:submit.prevent="submit">
                    <div class="form-group">
                        <label for="country">Which country will you be playing from?</label>
                        <select class="form-control" id="country" aria-describedby="country-help" v-model="country_code">
                            <option value="NONE">(would prefer not to disclose)</option>
                            <option v-for="country in countries" :value="country[0]">{{ country[1].name }}</option>
                        </select>
                        <span id="country-help" class="help-block">(optional)</span>
                    </div>
                    <div class="form-group" v-if="country_code !== 'NONE'">
                        <label for="country">What is your state, province, or region?</label>
                        <select class="form-control" id="country-region" aria-describedby="country-region-help" v-model="country_region_code">
                            <option value="NONE">(would prefer not to disclose)</option>
                            <option v-for="region in regions" :value="region[0]">{{ region[1].name }}</option>
                        </select>
                        <span id="country-region-help" class="help-block">(optional)</span>
                    </div>

                    <div class="form-group">
                        <label for="country">Which of the following describes you best?*</label>
                        <select class="form-control" id="level" v-model="level">
                            <option>Professional</option>
                            <option value="Undergraduate">In university (undergraduate)</option>
                            <option value="Graduate">In graduate school</option>
                            <option value="High School">In high school</option>
                        </select>
                    </div>

                    <div class="form-group" v-if="level === 'Professional'">
                        <label for="organization">Please choose your organization</label>
                        <select class="form-control" id="organization" v-model="organization">
                            <option value="NONE">(no affiliation)</option>
                            <option v-for="org in organizations" :value="org.organization_id">{{ org.name }} ({{org.type}})</option>
                        </select>

                        <label v-if="organization != 'NONE'" for="work-email">Please share your work email</label>
                        <input v-if="organization != 'NONE'" type="email" class="form-control" id="work-email" placeholder="Work email" aria-describedby="work-email-help" v-model="email" />
                        <span v-if="organization != 'NONE'" id="work-email-help" class="help-block">Used to verify your affiliation.</span>
                    </div>
                    <div class="form-group" v-if="level === 'Undergraduate' || level === 'Graduate'">
                        <label for="organization">Please choose your school</label>
                        <select class="form-control" id="organization" v-model="organization">
                            <option v-for="org in only_universities" :value="org.organization_id">{{ org.name }} ({{org.type}})</option>
                        </select>

                        <label for="school-email">Please share your school email</label>
                        <input type="email" class="form-control" id="school-email" placeholder="School email" aria-describedby="school-email-help" v-model="email" />
                        <span id="school-email-help" class="help-block">Used to verify your affiliation.</span>
                    </div>
                    <div class="form-group" v-if="level === 'High School'">
                        <label for="organization">Please choose your school</label>
                        <select class="form-control" id="organization" v-model="organization">
                            <option v-for="org in only_high_schools" :value="org.organization_id">{{ org.name }} ({{org.type}})</option>
                        </select>

                        <label v-if="organization != null" for="high-school-password">Great! Just enter your school password here:</label>
                        <input v-if="organization != null" type="text" class="form-control" id="high-school-password" placeholder="School password" aria-describedby="high-school-password-help" />
                        <span id="high-school-password-help" class="help-block">See your teacher if you don't know this</span>
                    </div>

                    <div class="form-group" v-if="organization !== 'NONE'">
                        <label for="org-name">Or, if you can't find your organization, share its name with us for approval</label>
                        <input type="text" class="form-control" id="org-name" placeholder="Name" aria-describedby="org-name-help" />
                        <span id="org-name-help" class="help-block"></span>
                    </div>

                    <div class="form-group has-error" v-if="error">
                        <span id="error-help" class="help-block">{{ error }}</span>
                    </div>

                    <button type="submit" class="btn btn-default">Submit</button>
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
              // TODO: Uncomment when finished implementing
              //  if (!me.is_new_user) {
              //      window.location.replace("/user?me");
              //  }
            });
            api.list_organizations().then((orgs) => {
               this.organizations = orgs;
            });
        },
    }
</script>

<style lang="scss" scoped>

</style>
