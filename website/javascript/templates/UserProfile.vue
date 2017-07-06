<template>
    <div class="row">
        <!-- TODO: indieweb markup -->
        <div class="col-md-4">
            <img class="img-responsive" :src="'https://github.com/' + user.username + '.png'" :alt="user.username">
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
                <h2>Game Feed</h2>

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
                            <td>{{ game.time_played | moment("calendar") }}</td>
                            <td>
                                <a v-for="player in Object.keys(game.players)" :href="'user?user_id=' + player" class="game-participant" >
                                    <img :alt="player" :src="profile_images[player]" />
                                </a>
                            </td>
                            <td>{{ game.map_width }}x{{ game.map_height }}</td>
                            <td>
                                <a :href="'play?game_id=' + game.game_id">
                                    {{ game.game_id }}
                                </a>
                            </td>
                        </tr>
                    </tbody>
                </table>
            </section>
        </div>
    </div>
</template>

<script>
    import * as api from "../api";

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
                profile_images: {},
            };
        },
        mounted: function() {
            const params = new URLSearchParams(window.location.search);
            let source;

            if (params.has("me")) {
                source = api.me();
            }
            else {
                const user_id = params.get("user_id");
                source = api.get_user(user_id);
            }

            source.then((user) => {
                this.user = user;
                $.get("http://35.190.3.178/api/v1/user/" + user.user_id.toString() + "/match")
                    .then((data) => {
                        this.games = data;
                        console.log(data);
                        for (let game of data) {
                            console.log(game);
                            for (let participant of Object.keys(game.players)) {
                                if (this.profile_images[participant]) continue;
                                this.profile_images[participant] = "loading";

                                api.get_user(participant).then((user) => {
                                    this.profile_images[participant] = `https://github.com/${user.username}.png`;
                                    this.$forceUpdate();
                                });
                            }
                        }
                    });
            });
        },
    }
</script>

<style lang="scss" scoped>
    .game-participant {
        img {
            height: 20px;
        }
    }
</style>
