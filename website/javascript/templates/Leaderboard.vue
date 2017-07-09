<template>
    <div>
        <div class="input-group">
            <input type="text" class="form-control" placeholder="Find username" v-model="username_filter" />
            <span class="input-group-btn">
                <button class="btn btn-default" type="button" v-on:click="update_filter" ><i class="fa fa-search" aria-hidden="true"></i></button>
            </span>
        </div><!-- /input-group -->
        <table class="table">
            <thead>
                <tr>
                    <th>#</th>
                    <th>Username</th>
                    <th>Organization</th>
                    <th>Level</th>
                    <th>Language</th>
                    <th>Points</th>
                </tr>
            </thead>
            <tbody>
                <tr v-for="player in leaderboard">
                    <td>{{ player.rank }}</td>
                    <td><a :href="'user?user_id=' + player.user_id">{{ player.username }}</a></td>
                    <td>{{ player.organization }}</td>
                    <td>{{ player.level }}</td>
                    <td>{{ player.language }}</td>
                    <td>{{ player.score }}</td>
                </tr>
            </tbody>
        </table>
    </div>
</template>

<script>
    import * as api from "../api";

    export default {
        name: "leaderboard",
        data: function() {
            return {
                leaderboard: [],
                username_filter: "",
            };
        },
        mounted: function() {
            this.update_filter();
        },
        methods: {
            update_filter: function() {
                let filters;
                if (this.username_filter.length > 0) {
                    filters = "username,=," + this.username_filter;
                }

                api.leaderboard(filters).then((leaderboard) => {
                    this.leaderboard = leaderboard;
                });
            }
        },
    }
</script>

<style lang="scss" scoped>

</style>
