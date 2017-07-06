<template>
    <table class="table">
        <thead>
            <tr>
                <th>#</th>
                <th>Username</th>
                <th>Organization</th>
                <th>Language</th>
                <th>Points</th>
            </tr>
        </thead>
        <tbody>
        <tr v-for="player in leaderboard">
            <td>{{ player.rank }}</td>
            <td><a :href="'user?user_id=' + player.user_id">{{ player.username }}</a></td>
            <td>{{ player.organization }}</td>
            <td>{{ player.language }}</td>
            <td>{{ player.points }}</td>
        </tr>
        </tbody>
    </table>
</template>

<script>
    export default {
        name: "leaderboard",
        data: function() {
            return {
                leaderboard: [],
            };
        },
        mounted: function() {
            $.get("http://35.190.3.178/api/v1/leaderboard").then((data) => {
                this.leaderboard = data;
                console.log(data);
            });
        },
    }
</script>

<style lang="scss" scoped>

</style>