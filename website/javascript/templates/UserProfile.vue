<template>
    <div class="row">
        <!-- TODO: indieweb markup -->
        <div class="col-md-4">
            <img class="img-responsive" src="" :alt="user.username">
            <h1>{{ user.username }}</h1>

            <p>{{ user.level }} at {{ user.organization }}</p>
        </div>
        <div class="col-md-8">
            <section>
                <h2>Stats Summary</h2>
                <p><b>Points:</b> {{ Math.round(user.score * 100) / 100 }}</p>
                <p><b>Games Played:</b>  {{ user.total_games_played }}</p>
                <p><b>Submissions:</b>  {{ user.total_submissions }}</p>
            </section>
            <section>
                <h2>Recent Games</h2>

                <table class="table">
                    <thead>
                        <tr>
                            <th>Played On</th>
                            <th>Opponents</th>
                            <th>Map Size</th>
                            <th>Watch</th>
                        </tr>
                    </thead>
                    <tbody>
                        <tr v-for="game in games">
                            <td>{{ game.time_played | moment("MMMM Do YYYY") }}</td>
                            <td>
                                <a v-for="player in Object.keys(game.players)" :href="'user?user_id=' + player">
                                    <img :alt="player" :src="player" />
                                </a>
                            </td>
                            <td>{{ game.map_width }} by {{ game.map_height }}</td>
                            <td>{{ game.game_id }}</td>
                        </tr>
                    </tbody>
                </table>
            </section>
        </div>
    </div>
</template>

<script>
    export default {
        name: "UserProfile",
        data: function() {
            return {
                user: {
                    "level": "",
                    "username": "",
                    "organization": "",
                    "points": "",
                    "num_games": "",
                },
                games: [],
            };
        },
        mounted: function() {
            const user_id = (new URLSearchParams(window.location.search)).get("user_id");
            $.get("http://35.190.3.178/api/v1/user/" + user_id.toString())
                .then((data) => {
                    this.user = data;
                    console.log(data);
                });
            $.get("http://35.190.3.178/api/v1/user/" + user_id.toString() + "/match")
                .then((data) => {
                    this.games = data;
                    console.log(data);
                });
        },
    }
</script>

<style lang="scss" scoped>

</style>