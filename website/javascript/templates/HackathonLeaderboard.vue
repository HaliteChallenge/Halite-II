<template>
    <div>
        <div class="page-header">
            <h1>Hackathon: {{ hackathon.title }}</h1>
        </div>
        <p>{{ hackathon.description }}</p>
        <dl>
            <dt>Start Date:</dt>
            <dd><time>{{ hackathon.start_date }}</time></dd>

            <dt>End Date:</dt>
            <dd><time>{{ hackathon.end_date }}</time></dd>
        </dl>
        <form v-on:submit="update_filter">
            <div class="input-group">
                <input type="text" class="form-control" placeholder="Find username" v-model="username_filter" />
                <span class="input-group-btn">
                    <button class="btn btn-default searchbarbutton" type="button" v-on:click="update_filter"><i class="fa fa-search" aria-hidden="true"></i></button>
                </span>
            </div><!-- /input-group -->
        </form>
        <table class="table">
            <thead>
            <tr>
                <th>#</th>
                <th>Username</th>
                <th>Organization</th>
                <th>Level</th>
                <th>Language</th>
                <th>Points</th>
                <th>Tier</th>
            </tr>
            </thead>
            <tbody>
            <tr v-for="player in leaderboard">
                <td>{{ player.local_rank }}</td>
                <td><a :href="'user?user_id=' + player.user_id">{{ player.username }}</a></td>
                <td>{{ player.organization }}</td>
                <td>{{ player.level }}</td>
                <td>{{ player.language }}</td>
                <td>{{ Math.round(100 * player.score) / 100 }}</td>
                <td>{{ player.local_tier }}</td>
            </tr>
            </tbody>
        </table>
    </div>
</template>

<script>
    import * as api from "../api";

    export default {
        name: "hackathon-leaderboard",
        data: function() {
            return {
                leaderboard: [],
                hackathon_id: null,
                hackathon: {
                    title: "",
                    start_date: null,
                    end_date: null,
                },
                username_filter: "",
            };
        },
        mounted: function() {
            const params = new URLSearchParams(window.location.search);
            this.hackathon_id = params.get("hackathon_id");
            this.update_filter();
            api.getHackathon(this.hackathon_id).then((hackathon) => {
                this.hackathon = hackathon;
            }, () => {

            });
        },
        methods: {
            update_filter: function(e) {
                if (e) e.preventDefault();

                let filters;
                if (this.username_filter.length > 0) {
                    filters = "username,=," + this.username_filter;
                }

                api.leaderboard(filters, 2).then((leaderboard) => {
                    this.leaderboard = leaderboard;
                });
            }
        },
    }
</script>

<style lang="scss" scoped>

</style>
