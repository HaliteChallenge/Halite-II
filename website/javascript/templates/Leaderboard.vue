<template>
    <div>
        <form v-on:submit="update_filter">
            <div class="input-group">
                <input type="text" class="form-control" placeholder="Find username" v-model="username_filter" />
                <span class="input-group-btn">
                    <button class="btn btn-default searchbarbutton" type="button" v-on:click="update_filter"><i class="fa fa-search" aria-hidden="true"></i></button>
                </span>
            </div><!-- /input-group -->
        </form>
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
                    <td><a :href="'user?user_id=' + player.user_id">{{ player.username }}</a></td>
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
    </div>
</template>

<script>
    import * as api from "../api";
    import {tierClass} from "../utils";

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
            tierClass: tierClass,
            update_filter: function(e) {
                if (e) e.preventDefault();

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
