<template>
    <div class="panel panel-default">
        <div class="panel-body">
            <p>
                Please provide us with some additional information to create your account.
            </p>

            <form>
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
                    <label for="work-email">Please share your work email</label>
                    <input type="email" class="form-control" id="work-email" placeholder="Work email" aria-describedby="work-email-help" />
                    <span id="work-email-help" class="help-block">Used to verify your affiliation.</span>
                </div>
                <div class="form-group" v-if="level === 'Undergraduate' || level === 'Graduate'">
                    <label for="school-email">Please share your school email</label>
                    <input type="email" class="form-control" id="school-email" placeholder="School email" aria-describedby="school-email-help" />
                    <span id="school-email-help" class="help-block">Used to verify your affiliation.</span>
                </div>
                <div class="form-group" v-if="level === 'High School'">
                    <label for="high-school">Please choose from existing high schools</label>
                    <select class="form-control" id="high-school" v-model="high_school">
                        <option>Basha High School</option>
                    </select>

                    <label v-if="high_school != null" for="high-school-password">Great! Just enter your school password here:</label>
                    <input v-if="high_school != null" type="text" class="form-control" id="high-school-password" placeholder="School password" aria-describedby="high-school-password-help" />
                    <span id="high-school-password-help" class="help-block">See your teacher if you don't know this</span>

                    <label for="high-school">Or, if you can't find your school, share its name with us for approval</label>
                    <input type="text" class="form-control" id="school-email" placeholder="School name" aria-describedby="school-email-help" />
                </div>
            </form>
        </div>
    </div>
</template>

<script>
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
                country_code: "NONE",
                country_region_code: "NONE",
                level: "Professional",
                high_school: null,
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
        },
        mounted: function() {

        },
    }
</script>

<style lang="scss" scoped>

</style>