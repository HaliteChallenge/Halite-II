<template>
    <div class="leaderboard-container">
        <form class="leaderboard-filter-form" v-on:submit="update_filter">
            <div class="form-header">
                <i class="xline xline-bottom"></i>
                <p class="t2 c-wht font-headline">FILTER</p>
                <div class="filter-handler">
                    <div class="handler-item">
                        <img class="handler-item-img" :src="`${baseUrl}/assets/images/simple-remove.svg`"/>
                        <span class="handler-item-text">Clear all</span>
                    </div>
                </div>
            </div>
            <div class="filter-group">
                <div class="input-group">
                    <input type="text" class="form-control ipt-username" placeholder="Search a user name" v-model="username_filter" />
                    <select class="form-control slt" v-model="tier_filter">
                        <option value="" selected>Tier</option>
                        <option value="1">Diamond</option>
                        <option value="2">Platinum</option>
                        <option value="3">Gold</option>
                        <option value="4">Silver</option>
                        <option value="5">Salt</option>
                    </select>
                    <select class="form-control slt">
                        <option value="" disabled selected>Organization</option>
                    </select>
                    <select class="form-control slt">
                        <option value="" disabled selected>Country</option>
                    </select>
                    <div>
                        <!-- <button class="btn btn-default searchbarbutton" type="button" v-on:click="update_filter"><i class="fa fa-search" aria-hidden="true"></i></button> -->
                        <button class="btn"><span>APPLY FILTER</span></button>
                    </div>
                </div><!-- /input-group -->
            </div>
        </form>
        <div class="leaderboard-summary">
            <div class="leaderboard-summary-item" v-for="s in summary">
                <img :src="s.img" />
                <div>
                    <p>{{s.number}}</p>
                    <p>{{s.name}}</p>
                </div>
            </div>
        </div>
        <table class="table table-leader">
            <thead>
                <tr>
                    <th>#</th>
                    <th>Username</th>
                    <th>Points</th>
                    <th>Tier</th>
                    <th>Academic Status</th>
                    <th>Organization</th>
                    <th>Language</th>
                </tr>
            </thead>
            <tbody>
                <tr v-for="player in leaderboard">
                    <td>{{ player.rank }}</td>
                    <td><a :href="'/user?user_id=' + player.user_id">{{ player.username }}</a></td>
                    <td>{{ Math.round(100 * player.score) / 100 }}</td>
                    <td>
                        <span :class="tierClass(player.tier)"></span>
                    </td>
                    <td>{{ player.level }}</td>
                    <td>{{ player.organization }}</td>
                    <td>{{ player.language }}</td>
                </tr>
            </tbody>
        </table>
        <div class="leaderboard-page">
            <HalitePagination
                :page="this.page"
                :lastPage="this.lastPage"
                :baseUrl="this.baseUrl"
                :changePage="this.changePage"
            />
        </div>
        </div>
    </div>
</template>

<script>
    import * as api from "../api";
    import HalitePagination from './Pagination.vue';
    import {tierClass} from "../utils";

    export default {
        name: "leaderboard",
        props: ['baseUrl', 'hackathonId'],
        components: {
            HalitePagination
        },
        data: function() {
            return {
                leaderboard: [],
                username_filter: "",
                tier_filter: "",
                organization_filter: "",
                page: 1,
                limit: 12,
                lastPage: 0,
                summary: [
                    {
                        img: `${this.baseUrl}/assets/images/leaderboard-grandMaster.svg`,
                        number: 23,
                        name: 'GrandMaster'
                    },
                    {
                        img: `${this.baseUrl}/assets/images/leaderboard-master.svg`,
                        number: 23,
                        name: 'Master'
                    },
                    {
                        img: `${this.baseUrl}/assets/images/leaderboard-professional.svg`,
                        number: 23,
                        name: 'Professional'
                    },
                    {
                        img: `${this.baseUrl}/assets/images/leaderboard-universityStudents.svg`,
                        number: 23,
                        name: 'University Students'
                    },
                    {
                        img: `${this.baseUrl}/assets/images/leaderboard-highSchoolStudents.svg`,
                        number: 23,
                        name: 'High School Students'
                    }
                ]
            };
        },
        mounted: function() {
            this.update_filter();
        },
        methods: {
            tierClass: tierClass,
            update_filter: function(e) {
                if (e) e.preventDefault();

                let filters = [];
                if (this.username_filter.length > 0) {
                    filters.push("username,=," + this.username_filter);
                }
                if (this.tier_filter.length > 0) {
                    filters.push("rank,=," + this.tier_filter);
                }
                filters = filters.length ? filters : null;
                if(this.lastPage <= 0) {
                    api.leaderboard(filters, this.hackathonId).then(leaderboard => {
                        if(leaderboard && leaderboard instanceof Array) {
                            this.lastPage = Math.ceil(leaderboard.length / this.limit)
                        }
                    })
                }
                api.leaderboard(filters, this.hackathonId, (this.page - 1) * this.limit, this.limit).then((leaderboard) => {
                    this.leaderboard = leaderboard;
                });
            },
            changePage: function(page) {
                let filters;
                if (this.username_filter.length > 0) {
                    filters = "username,=," + this.username_filter;
                }
                api.leaderboard(filters, this.hackathonId, (page - 1) * this.limit, this.limit).then((leaderboard) => {
                    this.leaderboard = leaderboard;
                    this.page = page;
                });
            },
            changePage: function(page) {
                let filters;
                if (this.username_filter.length > 0) {
                    filters = "username,=," + this.username_filter;
                }
                api.leaderboard(filters, this.hackathonId, (page - 1) * this.limit, this.limit).then((leaderboard) => {
                    this.leaderboard = leaderboard;
                    this.page = page;
                });
            }
        },
    }
</script>

<style lang="scss" scoped>

</style>
