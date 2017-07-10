<template>
    <div class="row">
        <!-- TODO: indieweb markup -->
        <div class="col-md-4">
            <img class="img-responsive" :src="'https://github.com/' + user.username + '.png'" :alt="user.username" onerror="this.src='https://upload.wikimedia.org/wikipedia/commons/thumb/a/ad/Placeholder_no_text.svg/2000px-Placeholder_no_text.svg.png'">
            <h1>{{ user.username }}</h1>

            <p>{{ user.level }} at {{ user.organization }}</p>
        </div>
        <div class="col-md-8">
            <section>
                <h2>Stats Summary</h2>
                <p><b>Points:</b> {{ Math.round(user.score * 100) / 100 }}</p>
                <p><b>Games Played:</b>  {{ user.total_games_played }}</p>
                <p><b>Submissions:</b>  {{ user.total_submissions }}</p>
                <p v-for="bot in bots">
                    Bot version {{ bot.version_number }} rank {{ bot.rank }} written in {{ bot.language }} with {{ bot.games_played }} games played
                    <span v-if="bot.compilation_status">(latest version compilation status: {{ bot.compilation_status }})</span>
                </p>
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
                                    <img :alt="player" :src="profile_images[player]" onerror="this.src='https://upload.wikimedia.org/wikipedia/commons/thumb/a/ad/Placeholder_no_text.svg/2000px-Placeholder_no_text.svg.png'" />
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
                <div class="btn-group" role="group" aria-label="Game Navigation">
                    <button
                        type="button"
                        class="btn btn-default"
                        :disabled="page === 0"
                        v-on:click="prev_page">Prev</button>
                    <button
                        type="button"
                        class="btn btn-default"
                        v-on:click="next_page">Next</button>
                </div>
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
                bots: [],
                profile_images: {},
                page: 0,
                offset: 0,
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
                if (user === null) {
                    window.location.replace(`${api.LOGIN_SERVER_URL}/github`);
                    return;
                }
                this.user = user;

                if (params.has("me")) {
                    window.history.replaceState(
                        {}, "",
                        `${window.location.origin}${window.location.pathname}?user_id=${user.user_id}`);
                }
                api.list_bots(user.user_id).then((bots) => {
                    this.bots = bots;
                });
                this.fetch();
            });
        },
        methods: {
            fetch: function() {
                return $.get(`${api.API_SERVER_URL}/user/${this.user.user_id}/match?order_by=desc,time_played&offset=${this.offset}`)
                 .then((data) => {
                     this.games = data;
                     for (let game of data) {
                         for (let participant of Object.keys(game.players)) {
                             if (this.profile_images[participant]) continue;
                             this.profile_images[participant] = "loading";

                             api.get_user(participant).then((user) => {
                                 this.profile_images[participant] = api.make_profile_image_url(user.username);
                                 this.$forceUpdate();
                             });
                         }
                     }
                 });
            },

            next_page: function() {
                this.offset += 10;
                this.fetch().then(() => {
                    this.page += 1;
                });
            },

            prev_page: function() {
                this.offset -= 10;
                this.fetch().then(() => {
                    this.page -= 1;
                });
            },
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
